
# Variables assigned in makefile must have = sign with spaces at both ends. The variables are inferenced uuuuusing $(var_name)
fftw_include_path = /gpfs/research/mecfd/Akash_Mittal/Softwares/fftw-3.3.10/include/
fftw_library_path = /gpfs/research/mecfd/Akash_Mittal/Softwares/fftw-3.3.10/lib/

compiler = gfortran

OBJ = mod_variables.o    \
	  mod_utility.o		 \
   	  mod_solve.o		 \
	  mod_initialize.o   \
   	  mod_readwrite.o    \
   	  main.o
# Has be compiled in the above sequence since later are dependent on the previous ones.
   	  
	  
compile: $(OBJ)
	$(compiler) -fopenmp -o run.exe *.o -I $(fftw_include_path) -L $(fftw_library_path) -lfftw3


%.o: %.f90
	$(compiler) -c $< -I $(fftw_include_path)
	# Include path is required while creating object file for the fortran file that includes the include file.


run:
	./run.exe


clean:
	rm *.o *.mod