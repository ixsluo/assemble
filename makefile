gcc = gfortran

target = double.x
# module file should be placed front.
obj = parameters.o math.o main.o read_file.o

${target} : ${obj}
	${gcc} -o ${target} ${obj}

%.o : %.f90
	${gcc} -c $< -o $*.o

clean:
	rm *.o *.mod
