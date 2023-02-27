# POC: ghc issue 22943

## Env

- ghc: 9.2.7
- cabal: 3.8.1.0

## Run

```
$ cabal run t1
Resolving dependencies...
Up to date
************************
************************
************************
************************
************************
Segmentation fault (core dumped)
```

```
$ gdb dist-newstyle/build/x86_64-linux/ghc-9.2.7/ttt-0.1.0.0/x/t1/build/t1/t1 core.623

(gdb) bt
#0  0x00000000004e4e14 in LOOKS_LIKE_INFO_PTR_NOT_NULL (p=12297829382473034410) at includes/rts/storage/ClosureMacros.h:253
#1  0x00000000004e4e63 in LOOKS_LIKE_INFO_PTR (p=12297829382473034410) at includes/rts/storage/ClosureMacros.h:258
#2  0x00000000004e4ea3 in LOOKS_LIKE_CLOSURE_PTR (p=0x4200f07000) at includes/rts/storage/ClosureMacros.h:264
#3  0x00000000004e625c in checkTSO (tso=0x4200ae2ba0) at rts/sm/Sanity.c:741
#4  0x00000000004e5be8 in checkClosure (p=0x4200ae2ba0) at rts/sm/Sanity.c:543
#5  0x00000000004e5d72 in checkHeapChain (bd=0x4200a03880) at rts/sm/Sanity.c:584
#6  0x00000000004e6a51 in checkGeneration (gen=0x1aee440, after_major_gc=true) at rts/sm/Sanity.c:948
#7  0x00000000004e6b4e in checkFullHeap (after_major_gc=true) at rts/sm/Sanity.c:973
#8  0x00000000004e6bcc in checkSanity (after_gc=true, major_gc=true) at rts/sm/Sanity.c:982
#9  0x00000000004e2048 in GarbageCollect (collect_gen=1, do_heap_census=false, is_overflow_gc=false, deadlock_detect=true,
    gc_type=2, cap=0x16de140, idle_cap=0x7facb8000bb0) at rts/sm/GC.c:959
#10 0x00000000004b5958 in scheduleDoGC (pcap=0x7facf9ffae38, task=0x7faddc000bb0, force_major=true, is_overflow_gc=false,
    deadlock_detect=true) at rts/Schedule.c:1860
#11 0x00000000004b4346 in scheduleDetectDeadlock (pcap=0x7facf9ffae38, task=0x7faddc000bb0) at rts/Schedule.c:953
#12 0x00000000004b2f73 in schedule (initialCapability=0x16de140, task=0x7faddc000bb0) at rts/Schedule.c:298
#13 0x00000000004b667e in scheduleWorker (cap=0x16de140, task=0x7faddc000bb0) at rts/Schedule.c:2647
#14 0x00000000004c930c in workerStart (task=0x7faddc000bb0) at rts/Task.c:445
#15 0x00007faf134bf609 in start_thread (arg=<optimized out>) at pthread_create.c:477
#16 0x00007faf13758133 in clone () at ../sysdeps/unix/sysv/linux/x86_64/clone.S:95
```
