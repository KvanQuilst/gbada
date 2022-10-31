GPRBUILD=gprbuild
GPRCLEAN=gprclean
GPR_FLAGS=

gbada:
	${GPRBUILD} ${GPR_FLAGS} -P gbada.gpr

clean:
	${GPRCLEAN} ${GPR_FLAGS} -P gbada.gpr
