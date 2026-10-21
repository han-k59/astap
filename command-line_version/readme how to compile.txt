How to compile ASTAP:

1) Install Lazarus from https://sourceforge.net/projects/lazarus/       (this will also install Free Pascal Compiler)


2a) Start Lazarus GUI.  Load astap.lpi or astap_linux.lip or astap_mac.lpi. Menu Run, Run or Compile

2b) Command line:

  Windows: 
     lazbuild -B astap_command_line.lpi

  Linux:  
    lazbuild -B astap_command_line.lpi

  Linux, PIE executable that you can run only via a terminal or a symlink:  
     lazbuild -B astap_command_line_linux_pie.lpi

  Mac:  
     lazbuild -B astap_command_line.lpi

Multi-core astap_cli:
  The command line solver uses all cores for FITS loading, star detection and the spiral search (unit_cli_parallel.pas).
  The results are identical to a single core run: -threads 1 and -threads 8 give the same star list and solution.
  -threads n or ASTAP_THREADS=n selects the core count (max 16, default the cores the process may run on).
  -progress runs the search on one core so the log stays in spiral order.
  Star database tiles are loaded once and shared by all threads. ASTAP_TILE_CACHE_MB=n limits that cache (default 256 MB).
  ASTAP_TIMING=1 prints the phase timings and the thread count to stderr.
