Compile & Run:

```sh
make asio_generic_server && ./asio_generic_server
```

Testing by ghz:

```sh
ghz -c 1 --connections=1 -n 1000 --insecure --proto proto/helloworld.proto -d '{"name": "x"}' --call helloworld.Greeter.SayHello 127.0.0.1:50051
```
