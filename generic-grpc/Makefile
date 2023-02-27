CXX = g++
CXXFLAGS += -std=c++17
LDFLAGS += -lpthread -lgrpc++ -lprotobuf -lgrpc++_reflection -lgpr

ASIO_CXXFLAGS = -fcoroutines -DAGRPC_BOOST_ASIO -DASIO_HAS_CO_AWAIT \
								-DBOOST_ASIO_HAS_STD_COROUTINE -DBOOST_ASIO_HAS_CO_AWAIT
ASIO_LDFLAGS = -lboost_coroutine -lboost_system

PROTOC = protoc
GRPC_CPP_PLUGIN = grpc_cpp_plugin
GRPC_CPP_PLUGIN_PATH ?= `which $(GRPC_CPP_PLUGIN)`

PROTOS_PATH = ./proto

vpath %.proto $(PROTOS_PATH)

all: grpc_async_server \
		 grpc_generic_server  grpc_generic_server2 \
		 asio_generic_server asio_generic_server2

grpc_async_server: helloworld.pb.o helloworld.grpc.pb.o grpc_async_server.o
	$(CXX) $^ $(LDFLAGS) -o $@

grpc_generic_server: helloworld.pb.o helloworld.grpc.pb.o grpc_generic_server.o
	$(CXX) $^ $(LDFLAGS) -o $@

grpc_generic_server2: helloworld.pb.o helloworld.grpc.pb.o grpc_generic_server2.o
	$(CXX) $^ $(LDFLAGS) -o $@

asio_generic_server: CXXFLAGS += $(ASIO_CXXFLAGS)
asio_generic_server: LDFLAGS += $(ASIO_LDFLAGS)
asio_generic_server: helloworld.pb.o helloworld.grpc.pb.o asio_generic_server.o
	$(CXX) $^ $(LDFLAGS) $(ASIO_CXXFLAGS) -o $@

asio_generic_server2: CXXFLAGS += $(ASIO_CXXFLAGS)
asio_generic_server2: LDFLAGS += $(ASIO_LDFLAGS)
asio_generic_server2: helloworld.pb.o helloworld.grpc.pb.o asio_generic_server2.o
	$(CXX) $^ $(LDFLAGS) -o $@

.PRECIOUS: %.grpc.pb.cc
%.grpc.pb.cc: %.proto
	$(PROTOC) -I $(PROTOS_PATH) --grpc_out=. --plugin=protoc-gen-grpc=$(GRPC_CPP_PLUGIN_PATH) $<

.PRECIOUS: %.pb.cc
%.pb.cc: %.proto
	$(PROTOC) -I $(PROTOS_PATH) --cpp_out=. $<

clean:
	rm -f *.o *.pb.cc *.pb.h \
		grpc_async_server \
		grpc_generic_server grpc_generic_server2 \
		asio_generic_server asio_generic_server2
