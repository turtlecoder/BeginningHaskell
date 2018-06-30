# Managing Cloud Topologies

[Tutorial on Cloud Topologies in Cloud Haskell](http://haskell-distributed.github.io/tutorials/2ch.html)

- To launch the tutorial on cloud topologies, Do the following

  1. cabal build/stack build
  2. Launch multiple slave processes
  
	  ```
	  $ .stack-work/install/x86_64-osx/lts-9.21/8.0.2/bin/master-slave-config slave 127.0.0.1 8001 &
	  $ .stack-work/install/x86_64-osx/lts-9.21/8.0.2/bin/master-slave-config slave 127.0.0.1 8002 &
	  $ .stack-work/install/x86_64-osx/lts-9.21/8.0.2/bin/master-slave-config slave 127.0.0.1 8003 &
	  ```
	  
  3. Using `tree -s master-slave-config`, you should see the following process tree
  ```
   \-+= 22001 hkhanhex -bash
     |--= 35302 hkhanhex .stack-work/install/x86_64-osx/lts-9.21/8.0.2/bin/master-slave-config slave 127.0.0.1 8002
     |--= 35597 hkhanhex .stack-work/install/x86_64-osx/lts-9.21/8.0.2/bin/master-slave-config slave 127.0.0.1 8003
     \--= 35773 hkhanhex .stack-work/install/x86_64-osx/lts-9.21/8.0.2/bin/master-slave-config slave 127.0.0.1 8004

  ```
  
  
