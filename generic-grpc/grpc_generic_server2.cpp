#include <iostream>
#include <memory>
#include <string>
#include <thread>

#include <grpc/support/log.h>
#include <grpcpp/generic/async_generic_service.h>
#include <grpcpp/grpcpp.h>

#include "helloworld.grpc.pb.h"

using grpc::ByteBuffer;
using grpc::ServerCompletionQueue;
using grpc::ServerContext;
using grpc::Slice;
using grpc::Status;

using helloworld::HelloReply;
using helloworld::HelloRequest;

struct ServerData {
  std::vector<std::unique_ptr<ServerCompletionQueue>> cq_;
  grpc::AsyncGenericService service_;
  std::unique_ptr<grpc::Server> server_;
  std::vector<std::thread> server_threads_;
};

ServerData* new_server(int parallelism) {
  ServerData* server_data = new ServerData;
  std::string server_address("0.0.0.0:50051");

  grpc::ServerBuilder builder;
  // Listen on the given address without any authentication mechanism.
  builder.AddListeningPort(server_address, grpc::InsecureServerCredentials());
  // Register "service_" as the instance through which we'll communicate with
  // clients. In this case it corresponds to an *asynchronous* service.
  builder.RegisterAsyncGenericService(&server_data->service_);
  for (int i = 0; i < parallelism; i++) {
    server_data->cq_.emplace_back(builder.AddCompletionQueue());
  }
  // Finally assemble the server.
  server_data->server_ = builder.BuildAndStart();
  std::cout << "Server listening on " << server_address << std::endl;

  return server_data;
}

void shutdown_server(ServerData* server) {
  server->server_->Shutdown();
  // Always shutdown the completion queue after the server.
  for (auto& cq : server->cq_) {
    cq->Shutdown();
  }
  delete server;
}

enum class CallStatus { CREATE, READ, WRITE, FINISH, DESTROY };

// Class encompasing the state and logic needed to serve a request.
class CallData {
public:
  CallData(grpc::AsyncGenericService* service, ServerCompletionQueue* cq)
      : service_(service), cq_(cq), stream_(&ctx_),
        status_(CallStatus::CREATE) {
    service_->RequestCall(&ctx_, &stream_, cq_, cq_, this);
    status_ = CallStatus::READ;
  }

  void Proceed() {
    if (status_ == CallStatus::READ) {
      // Spawn a new CallData instance to serve new clients while we process
      // the one for this CallData. The instance will deallocate itself as
      // part of its FINISH state.
      new CallData(service_, cq_);

      // -- Wait for the request message
      stream_.Read(&buffer, this);
      status_ = CallStatus::WRITE;
    } else if (status_ == CallStatus::WRITE) {
      // -- Deserialize the request message
      grpc::ProtoBufferReader reader{&buffer};
      GPR_ASSERT(request_.ParseFromZeroCopyStream(&reader));

      // -- Serialize the response message
      buffer.Clear();
      reply_.set_message("Hello " + request_.name());
      const auto message_byte_size = static_cast<int>(reply_.ByteSizeLong());
      grpc::ProtoBufferWriter writer{&buffer, message_byte_size,
                                     message_byte_size};
      reply_.SerializeToZeroCopyStream(&writer);

      // -- Write the response message and finish this RPC with OK
      stream_.Write(buffer, this);

      status_ = CallStatus::FINISH;
    } else if (status_ == CallStatus::FINISH) {
      stream_.Finish(Status::OK, this);
      status_ = CallStatus::DESTROY;
    } else {
      GPR_ASSERT(status_ == CallStatus::DESTROY);
      delete this;
    }
  }

private:
  grpc::AsyncGenericService* service_;
  grpc::GenericServerContext ctx_;
  ServerCompletionQueue* cq_;

  ByteBuffer buffer;

  // What we get from the client.
  HelloRequest request_;
  // What we send back to the client.
  HelloReply reply_;

  grpc::GenericServerAsyncReaderWriter stream_;

  CallStatus status_; // The current serving state.
};

// This can be run in multiple threads if needed.
void HandleRpcs(grpc::AsyncGenericService* service, ServerCompletionQueue* cq) {
  // Spawn a new CallData instance to serve new clients.
  new CallData(service, cq);

  void* tag; // uniquely identifies a request.
  bool ok;
  while (true) {
    GPR_ASSERT(cq->Next(&tag, &ok));
    GPR_ASSERT(ok);
    static_cast<CallData*>(tag)->Proceed();
  }
}

int main(int argc, char** argv) {
  const auto env = std::getenv("GRPC_SERVER_CPUS");
  const auto parallelism =
      env ? std::atoi(env) : std::thread::hardware_concurrency();

  auto server = new_server(parallelism);

  // Proceed to the server's main loop.
  for (int i = 0; i < parallelism; i++) {
    server->server_threads_.emplace_back(std::thread(
        [server, i] { HandleRpcs(&server->service_, server->cq_[i].get()); }));
  }

  for (auto& thread : server->server_threads_) {
    thread.join();
  }

  shutdown_server(server);
  return 0;
}
