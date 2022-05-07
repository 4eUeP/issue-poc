#include "helloworld.grpc.pb.h"

#include <forward_list>
#include <iostream>
#include <thread>
#include <vector>

#include <agrpc/asioGrpc.hpp>
#include <boost/asio/spawn.hpp>
#include <google/protobuf/message_lite.h>
#include <grpcpp/generic/async_generic_service.h>
#include <grpcpp/generic/generic_stub.h>
#include <grpcpp/server.h>
#include <grpcpp/server_builder.h>

namespace asio = boost::asio;
using helloworld::HelloReply;
using helloworld::HelloRequest;

template <class CompletionToken = agrpc::DefaultCompletionToken>
auto request(grpc::AsyncGenericService& service,
             grpc::GenericServerContext& server_context,
             grpc::GenericServerAsyncReaderWriter& reader_writer,
             CompletionToken&& token = {}) {
  return agrpc::grpc_initiate(
      [&](agrpc::GrpcContext& grpc_context, void* tag) {
        auto* const cq = grpc_context.get_server_completion_queue();
        service.RequestCall(&server_context, &reader_writer, cq, cq, tag);
      },
      std::forward<CompletionToken>(token));
}

void spawn_accept_loop(agrpc::GrpcContext& grpc_context,
                       grpc::AsyncGenericService& service) {
  asio::co_spawn(
      grpc_context,
      [&]() -> asio::awaitable<void> {
        grpc::GenericServerContext server_context;
        grpc::GenericServerAsyncReaderWriter reader_writer{&server_context};
        if (!co_await request(service, server_context, reader_writer)) {
          co_return;
        }

        spawn_accept_loop(grpc_context, service);

        grpc::ByteBuffer buffer;
        co_await agrpc::read(reader_writer, buffer);

        if (server_context.method() == "/helloworld.Greeter/SayHello") {
          HelloRequest request;
          HelloReply reply;

          grpc::ProtoBufferReader reader{&buffer};
          request.ParseFromZeroCopyStream(&reader);

          // printf("%s \n", request.name().c_str());

          buffer.Clear();

          reply.set_message("Hello " + request.name());
          const auto reply_byte_size = static_cast<int>(reply.ByteSizeLong());
          grpc::ProtoBufferWriter writer{&buffer, reply_byte_size,
                                         reply_byte_size};
          reply.SerializeToZeroCopyStream(&writer);

          co_await agrpc::write(reader_writer, buffer);

          co_await agrpc::finish(reader_writer, grpc::Status::OK);
        } else {
          throw std::runtime_error("Unsupported method");
        }
      },
      [](std::exception_ptr ep, auto&&...) {
        if (ep) {
          std::rethrow_exception(ep);
        }
      });
}

int main() {
  std::string server_address("0.0.0.0:50051");

  grpc::ServerBuilder builder;
  std::unique_ptr<grpc::Server> server;
  grpc::AsyncGenericService service;

  builder.AddListeningPort(server_address, grpc::InsecureServerCredentials());
  builder.RegisterAsyncGenericService(&service);

  const auto env = std::getenv("GRPC_SERVER_CPUS");
  const auto parallelism =
      env ? std::atoi(env) : std::thread::hardware_concurrency();
  std::forward_list<agrpc::GrpcContext> grpc_contexts;
  for (size_t i = 0; i < parallelism; ++i) {
    grpc_contexts.emplace_front(builder.AddCompletionQueue());
  }

  server = builder.BuildAndStart();
  std::cout << "Server listening on " << server_address << std::endl;

  std::vector<std::thread> threads;
  threads.reserve(parallelism);
  for (size_t i = 0; i < parallelism; ++i) {
    threads.emplace_back([&, i] {
      auto& grpc_context = *std::next(grpc_contexts.begin(), i);
      spawn_accept_loop(grpc_context, service);
      grpc_context.run();
    });
  }

  for (auto& thread : threads) {
    thread.join();
  }

  server->Shutdown();
}
