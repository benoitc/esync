PROJECT = esync
PROJECT_DESCRIPTION = simple tool to sync files
PROJECT_VERSION = 0.0.2

LOCAL_DEPS = crypto worker_pool
DEPS = worker_pool


dep_worker_pool = git https://github.com/inaka/worker_pool.git 1.0.4

include erlang.mk
