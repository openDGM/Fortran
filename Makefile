LDFLAGS=-I. -I/usr/include/ -L/usr/lib -lnetcdf -lnetcdff

netcdf_reader : netcdf_reader.f90 netcdf_handle.o
	gfortran netcdf_reader.f90 netcdf_handle.o $(LDFLAGS) -o $@

netcdf_handle.o : netcdf_handle.f90
	gfortran -c $< $(LDFLAGS) 

clean :
	rm *.o *.mod netcdf_reader
