gcc = gfortran

target = double.x
obj = parameters.f90  main.f90 read_file.f90 math.f90

${target} : ${obj}
	${gcc} -o ${target} ${obj}

clean:
	rm *.o *.mod
