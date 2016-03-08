program netcdf_test
  use mpi
  use netcdf
  use netcdf_handle
  implicit none

  integer :: errc, mpi_rank, mpi_size, i
  integer, dimension(12, 4) :: raw_data

  ! Note the integer kinds of these variables!
  integer(kind=hsize_t) :: dims(2), counts(2)
  integer(kind=hssize_t) :: offsets(2)
  
  call mpi_init(errc)

  call mpi_comm_size(MPI_COMM_WORLD, mpi_size, errc)
  call mpi_comm_rank(MPI_COMM_WORLD, mpi_rank, errc)

  ! Open a new netcdf file in parallel
  type(nc_file) :: nc_test
  integer :: stat

  stat = nc_test%nc_create('test.nc', NF90_HDF5, MPI_COMM_WORLD, MPI_INFO_NULL)
  stat = nc_test%nc_defdim('x',12)
  stat = nc_test%nc_defdim('y',4)
  stat = nc_test%nc_defvar('raw_data',NF90_INT,(/1,2/))
  stat = nc_test%nc_enddef()

  ! Set up the test data
  dims = [12, 4]
  counts(1) = dims(1) / mpi_size
  counts(2) = dims(2)
  offsets(1) = mpi_rank * counts(1)
  offsets(2) = 0
  raw_data = reshape([(i, i = 1, product(dims))] + mpi_rank * 100, dims)

  call nc_test%nc_write('raw_data', raw_data, stat)

  stat = nc_test%nc_close()
  call mpi_finalize(errc)

end program netcdf_test
