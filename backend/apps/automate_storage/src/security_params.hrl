%% Password hashing parameters
-ifndef (TEST).
-define(PASSWORD_HASHING_PARALLELISM, 1).   % Not more than one thread will be used for this at once
-define(PASSWORD_HASHING_OPS_LIMIT, 8).     % Default value from argon2_elixir
-define(PASSWORD_HASHING_MEM_LIMIT, 32768). % 32MiB. Was libsodium:memlimit_interactive()
-define(PASSWORD_HASHING_HASHLEN, 32).      % Default value from argon2_elixir
-define(PASSWORD_HASHING_SALTLEN, 16).      % Default value from argon2_elixir
-else.
%% Parameters for testing environment. Make the login and registering significatively lighter.
-define(PASSWORD_HASHING_PARALLELISM, 1).   % Not more than one thread will be used for this at once
-define(PASSWORD_HASHING_OPS_LIMIT, 1).     % Default value from argon2_elixir
-define(PASSWORD_HASHING_MEM_LIMIT, 1024). % 32MiB. Was libsodium:memlimit_interactive()
-define(PASSWORD_HASHING_HASHLEN, 32).      % Default value from argon2_elixir
-define(PASSWORD_HASHING_SALTLEN, 16).      % Default value from argon2_elixir
-endif.

%% Token generation parameters
-define(KEY_RANDOM_LENGTH, 30).
