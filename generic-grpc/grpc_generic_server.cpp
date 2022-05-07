#include <iostream>
#include <memory>
#include <string>
#include <thread>

#include <grpc/support/log.h>
#include <grpcpp/generic/async_generic_service.h>
#include <grpcpp/grpcpp.h>

#include "helloworld.grpc.pb.h"

using grpc::ByteBuffer;
using grpc::Server;
using grpc::ServerBuilder;
using grpc::ServerCompletionQueue;
using grpc::ServerContext;
using grpc::Slice;
using grpc::Status;

using helloworld::HelloReply;
using helloworld::HelloRequest;

// Let's implement a tiny state machine with the following states.
enum class CallStatus { CREATE, READ, PROCESS, FINISH, DESTROY };

class ServerImpl final {
public:
  ~ServerImpl() {
    server_->Shutdown();
    // Always shutdown the completion queue after the server.
    for (auto& cq : cq_)
      cq->Shutdown();
  }

  // There is no shutdown handling in this code.
  void Run() {
    std::string server_address("0.0.0.0:50051");

    ServerBuilder builder;
    // Listen on the given address without any authentication mechanism.
    builder.AddListeningPort(server_address, grpc::InsecureServerCredentials());
    // Register "service_" as the instance through which we'll communicate with
    // clients. In this case it corresponds to an *asynchronous* service.
    builder.RegisterAsyncGenericService(&service_);
    // Get hold of the completion queue used for the asynchronous communication
    // with the gRPC runtime.
    const auto env = std::getenv("GRPC_SERVER_CPUS");
    const auto parallelism =
        env ? std::atoi(env) : std::thread::hardware_concurrency();
    for (int i = 0; i < parallelism; i++) {
      cq_.emplace_back(builder.AddCompletionQueue());
    }
    // Finally assemble the server.
    server_ = builder.BuildAndStart();
    std::cout << "Server listening on " << server_address << std::endl;

    // Proceed to the server's main loop.
    for (int i = 0; i < parallelism; i++) {
      server_threads_.emplace_back(
          std::jthread([this, i] { this->HandleRpcs(i); }));
    }

    std::this_thread::sleep_until(
        std::chrono::time_point<std::chrono::system_clock>::max());
  }

private:
  // Class encompasing the state and logic needed to serve a request.
  class CallData {
  public:
    // Take in the "service" instance (in this case representing an asynchronous
    // server) and the completion queue "cq" used for asynchronous communication
    // with the gRPC runtime.
    CallData(grpc::AsyncGenericService* service, ServerCompletionQueue* cq)
        : service_(service), cq_(cq), stream_(&ctx_),
          status_(CallStatus::CREATE) {
      // Invoke the serving logic right away.
      Proceed();
    }

    void Proceed() {
      if (status_ == CallStatus::CREATE) {
        status_ = CallStatus::READ;

        service_->RequestCall(&ctx_, &stream_, cq_, cq_, this);
      } else if (status_ == CallStatus::READ) {
        // Spawn a new CallData instance to serve new clients while we process
        // the one for this CallData. The instance will deallocate itself as
        // part of its FINISH state.
        new CallData(service_, cq_);

        // -- Wait for the request message
        stream_.Read(&buffer, this);
        status_ = CallStatus::PROCESS;
      } else if (status_ == CallStatus::PROCESS) {
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
    void* got_tag;
    bool ok;

    // What we get from the client.
    HelloRequest request_;
    // What we send back to the client.
    HelloReply reply_;

    // ServerAsyncResponseWriter<HelloReply> responder_;
    grpc::GenericServerAsyncReaderWriter stream_;

    CallStatus status_; // The current serving state.
  };

  // This can be run in multiple threads if needed.
  void HandleRpcs(int i) {
    // Spawn a new CallData instance to serve new clients.
    new CallData(&service_, cq_[i].get());
    void* tag; // uniquely identifies a request.
    bool ok;
    while (true) {
      // Block waiting to read the next event from the completion queue. The
      // event is uniquely identified by its tag, which in this case is the
      // memory address of a CallData instance.
      // The return value of Next should always be checked. This return value
      // tells us whether there is any kind of event or cq_ is shutting down.
      GPR_ASSERT(cq_[i]->Next(&tag, &ok));
      // GPR_ASSERT(ok);
      static_cast<CallData*>(tag)->Proceed();
    }
  }

  std::vector<std::unique_ptr<ServerCompletionQueue>> cq_;
  grpc::AsyncGenericService service_;
  std::unique_ptr<Server> server_;
  std::vector<std::jthread> server_threads_;
};

int main(int argc, char** argv) {
  ServerImpl server;
  server.Run();

  return 0;
}
