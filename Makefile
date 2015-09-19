PROJECT = oneup

CC = g++ 
CFLAGS ?= -O3 -std=c++11 -finline-functions -Wall

include erlang.mk

# Uncomment this to build oneup with test functions.
# ERLC_OPTS += -DTEST=1

deps/horse:
	git clone -n -- https://github.com/andytill/horse $(DEPS_DIR)/horse
	cd $(DEPS_DIR)/horse ; git checkout -q master
	$(MAKE) -C $(DEPS_DIR)/horse

perfs: ERLC_OPTS += -DPERF=1 +'{parse_transform, horse_autoexport}'
perfs: clean deps deps/horse app
	$(gen_verbose) erl \
		-noshell \
		+K true +A 10 +sbwt very_long +swt very_low \
		+Mulmbcs 32767 +Mumbcgs 1 +Musmbcs 2047 \
		-pa ebin deps/horse/ebin \
		-eval 'horse:app_perf($(PROJECT)), init:stop().'