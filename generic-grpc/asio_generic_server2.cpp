#include "helloworld.grpc.pb.h"

#include <agrpc/asioGrpc.hpp>
#include <boost/asio/spawn.hpp>
#include <grpcpp/server.h>
#include <grpcpp/server_builder.h>

#include <forward_list>
#include <iostream>
#include <thread>
#include <vector>

namespace asio = boost::asio;
using helloworld::HelloReply;
using helloworld::HelloRequest;

struct GenericRequestHandler {
  using executor_type = agrpc::GrpcContext::executor_type;

  agrpc::GrpcContext& grpc_context;

  static void
  handle_generic_request(grpc::GenericServerContext& server_context,
                         grpc::GenericServerAsyncReaderWriter reader_writer,
                         const asio::yield_context& yield) {

    if (server_context.method() == "/helloworld.Greeter/SayHello") {
      HelloRequest request;
      HelloReply reply;
      grpc::ByteBuffer buffer;

      // -- Wait for the request message
      agrpc::read(reader_writer, buffer, yield);

      // -- Deserialize the request message
      grpc::ProtoBufferReader reader{&buffer};
      request.ParseFromZeroCopyStream(&reader);

      // -- Serialize the response message
      buffer.Clear();
      reply.set_message("Hello " + request.name());
      const auto message_byte_size = static_cast<int>(reply.ByteSizeLong());
      grpc::ProtoBufferWriter writer{&buffer, message_byte_size,
                                     message_byte_size};
      reply.SerializeToZeroCopyStream(&writer);

      // -- Write the response message and finish this RPC with OK
      agrpc::write_and_finish(reader_writer, buffer, grpc::WriteOptions{},
                              grpc::Status::OK, yield);
    } else {
      throw std::runtime_error("Unsupported method");
    }
  }

  void operator()(agrpc::GenericRepeatedlyRequestContext<>&& context) const {
    asio::spawn(grpc_context,
                [context = std::move(context)](asio::yield_context yield) {
                  handle_generic_request(context.server_context(),
                                         context.responder(), yield);
                });
  }

  auto get_executor() const noexcept { return grpc_context.get_executor(); }
};

int main() {
  std::string server_address("0.0.0.0:50051");

  grpc::ServerBuilder builder;
  std::unique_ptr<grpc::Server> server;
  grpc::AsyncGenericService service;

  const auto env = std::getenv("GRPC_SERVER_CPUS");
  const auto parallelism =
      env ? std::atoi(env) : std::thread::hardware_concurrency();
  std::forward_list<agrpc::GrpcContext> grpc_contexts;
  for (size_t i = 0; i < parallelism; ++i) {
    grpc_contexts.emplace_front(builder.AddCompletionQueue());
  }

  builder.AddListeningPort(server_address, grpc::InsecureServerCredentials());
  builder.RegisterAsyncGenericService(&service);
  server = builder.BuildAndStart();
  std::cout << "Server listening on " << server_address << std::endl;

  std::vector<std::thread> threads;
  threads.reserve(parallelism);
  for (size_t i = 0; i < parallelism; ++i) {
    threads.emplace_back([&, i] {
      auto& grpc_context = *std::next(grpc_contexts.begin(), i);
      agrpc::repeatedly_request(service, GenericRequestHandler{grpc_context});
      grpc_context.run();
    });
  }

  for (auto& thread : threads) {
    thread.join();
  }

  server->Shutdown();
}
