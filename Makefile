PROJECT = esync 
PROJECT_DESCRIPTION = simple tool to sync files
PROJECT_VERSION = 0.0.1

LOCAL_DEPS = crypto jobs
DEPS = jobs

dep_jobs = git https://github.com/uwiger/jobs.git 0.3

include erlang.mk
