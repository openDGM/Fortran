program f90tst_parallel
  use netcdf
  use netcdf_handle
  use mpi
  implicit none
  ! This is the name of the data file we will create.
  character (len = *), parameter :: FILE_NAME = "f90tst_parallel.nc"

  integer, parameter :: MAX_DIMS = 2
  integer, parameter :: NX = 16, NY = 16
  integer, parameter :: NUM_PROC = 4
  integer :: ncid, varid, dimids(MAX_DIMS), chunksizes(MAX_DIMS), chunksizes_in(MAX_DIMS)
  integer :: x_dimid, y_dimid, contig
  integer :: data_out(NY / 2, NX / 2), data_in(NY / 2, NX / 2)
  integer :: mode_flag
  integer :: nvars, ngatts, ndims, unlimdimid, file_format
  integer :: x, y, retval
  integer :: p, my_rank, ierr
  integer :: start(MAX_DIMS), count(MAX_DIMS)

  call MPI_Init(ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, p, ierr)

  if (my_rank .eq. 0) then
     print *, ' '
     print *, '*** Testing netCDF-4 parallel I/O from Fortran 90.'
  endif

  ! There must be 4 procs for this test.
  if (p .ne. 4) then
     print *, 'Sorry, this test program must be run on four processors.'
     stop 2 
  endif

  ! Create some pretend data.
  do x = 1, NX / 2
     do y = 1, NY / 2
        data_out(y, x) = my_rank
     end do
  end do

  type(nc_file) :: nc_test
  integer :: stat

  stat = nc_test%nc_create(FILE_NAME, NF90_HDF5, MPI_COMM_WORLD, MPI_INFO_NULL)
  stat = nc_test%nc_defdim('x',12)
  stat = nc_test%nc_defdim('y',4)
  stat = nc_test%nc_defvar('raw_data',NF90_INT,(/1,2/))
  stat = nc_test%nc_enddef()

  ! Determine what part of the variable will be written for this
  ! processor. It's a checkerboard decomposition.
  count = (/ NX / 2, NY / 2 /)
  if (my_rank .eq. 0) then
     start = (/ 1, 1 /)
  else if (my_rank .eq. 1) then
     start = (/ NX / 2 + 1, 1 /)
  else if (my_rank .eq. 2) then
     start = (/ 1, NY / 2 + 1 /)
  else if (my_rank .eq. 3) then
     start = (/ NX / 2 + 1, NY / 2 + 1 /)
  endif

  call nc_test%nc_write('raw_data', data_out, nc_stat, start, count)
  stat = nc_test%nc_close()

  call MPI_Finalize(ierr)

  if (my_rank .eq. 0) print *,'*** SUCCESS!'
end program f90tst_parallel
