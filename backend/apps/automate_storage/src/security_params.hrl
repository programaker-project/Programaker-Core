%% Password hashing parameters
-define(PASSWORD_HASHING_PARALLELISM, 1).   % Not more than one thread will be used for this at once
-define(PASSWORD_HASHING_OPS_LIMIT, 8).     % Default value from argon2_elixir
-define(PASSWORD_HASHING_MEM_LIMIT, 32768). % 32MiB. Was libsodium:memlimit_interactive()
-define(PASSWORD_HASHING_HASHLEN, 32).      % Default value from argon2_elixir
-define(PASSWORD_HASHING_SALTLEN, 16).      % Default value from argon2_elixir
