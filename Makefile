PROJECT = oneup

CC = g++ 
CFLAGS ?= -O3 -std=c++11 -finline-functions -Wall

include erlang.mk

deps/horse:
	git clone -n -- https://github.com/andytill/horse $(DEPS_DIR)/horse
	cd $(DEPS_DIR)/horse ; git checkout -q master
	$(MAKE) -C $(DEPS_DIR)/horse

perfs: ERLC_OPTS += -DPERF=1 +'{parse_transform, horse_autoexport}'
perfs: clean deps deps/horse app
	$(gen_verbose) erl -noshell -pa ebin deps/horse/ebin \
		-eval 'horse:app_perf($(PROJECT)), init:stop().'