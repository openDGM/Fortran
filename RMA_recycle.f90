!-- Program that creates a test environment with array structures similar to the computational grid of PALM in a parallel run with 2D grid decomposition

!-- Declare header modules
module array_bounds
  implicit none
  integer  ::  nxlg, nxl, nxrg, nxr, nysg, nys, nyng, nyn, nzb, nzt
end module array_bounds

module mpi_helper
  implicit none
  integer  ::  my_rank, ierr, comm2d, row_type, column_type
  integer, dimension(2)      ::  dims, coords
end module mpi_helper

module filter
  implicit none

  contains

  subroutine filter1D(profiles)
  real, dimension(:,:) :: profiles
  integer :: NI,NJ
  integer :: j
  NI = size(profiles,1)
  NJ = size(profiles,2)

  do j = 1,NJ
    profiles(:,j) = sum(profiles(:,j)/NI)
  end do

  end subroutine filter1D

end module

program rma_recycle

  use mpi
  use mpi_helper
  use array_bounds

  implicit none
!-- Not exactly like PALM since PALM uses 64bit precision
  real, allocatable, dimension(:,:,:)    ::  u,v
 
  call init
  call recycle

!-- Print out the filtered (i.e. spanwise averageed) that has been copied to the inflow ghoast layer.
  if(my_rank==0) then
    write(*,*) u(:,:,nxlg)
  end if

  call finalize
 
  contains

  subroutine init
    use array_bounds
    use mpi_helper

    implicit none
    call MPI_Init(ierr)

!-- Hard code 4x4 PE-grid
    dims(1) = 4
    dims(2) = 4

!-- Create the 2d PE-grid
    call MPI_Cart_Create(mpi_comm_world, 2, dims, (/ .false. , .false. /), .true., comm2d, ierr)
    call MPI_Comm_rank(comm2d, my_rank, ierr)
    call MPI_Cart_Coords(comm2d, my_rank, 2, coords, ierr)

!-- Declare MPI-User Types to communicate rows and columns of 3D arrays. A user type for the third dimension (x) is missing. 
    call MPI_TYPE_VECTOR(1,32,32,MPI_INT,column_type,ierr)
    call MPI_TYPE_COMMIT(column_type,ierr)

    call MPI_TYPE_VECTOR(4,1,32,MPI_INT,row_type,ierr)
    call MPI_TYPE_COMMIT(row_type,ierr)

!-- Create array bounds for local arrays on PE. Arrays are size 32x6x6 with 32x4x4 of data and 1 ghost layer in x and y.
    nxl = coords(1)*4+1
    nxlg  = nxl-1
    nxr  = nxl+3
    nxrg = nxl+4

    nys = coords(2)*4+1
    nysg  = nys-1
    nyn  = nys+3
    nyng = nys+4

    nzb  = 1
    nzt  = 32

    allocate(u(nzb:nzt,nysg:nyng,nxlg:nxrg), v(nzb:nzt,nysg:nyng,nxlg:nxrg))

!-- Initialize u and v data with PE rank.
    u(:,nys:nyn,nxl:nxr) = my_rank;
    v = 2*u;

!-- Horizontal slice of u(1,:,:) over all PEs then looks like this
! 
!   0  0  0  0  0  0 | 0  0  0  0  0  0 | 0  0  0  0  0  0 | 0  0  0  0  0  0
!   0  0  0  0  0  0 | 0  1  1  1  1  0 | 0  2  2  2  2  0 | 0  3  3  3  3  0
!   0  0  0  0  0  0 | 0  1  1  1  1  0 | 0  2  2  2  2  0 | 0  3  3  3  3  0
!   0  0  0  0  0  0 | 0  1  1  1  1  0 | 0  2  2  2  2  0 | 0  3  3  3  3  0
!   0  0  0  0  0  0 | 0  1  1  1  1  0 | 0  2  2  2  2  0 | 0  3  3  3  3  0
!   0  0  0  0  0  0 | 0  0  0  0  0  0 | 0  0  0  0  0  0 | 0  0  0  0  0  0
!   -----------------|------------------|------------------|-----------------
!   0  0  0  0  0  0 | 0  0  0  0  0  0 | 0  0  0  0  0  0 | 0  0  0  0  0  0
!   0  4  4  4  4  0 | 0  5  5  5  5  0 | 0  6  6  6  6  0 | 0  7  7  7  7  0
!   0  4  4  4  4  0 | 0  5  5  5  5  0 | 0  6  6  6  6  0 | 0  7  7  7  7  0
!   0  4  4  4  4  0 | 0  5  5  5  5  0 | 0  6  6  6  6  0 | 0  7  7  7  7  0
!   0  4  4  4  4  0 | 0  5  5  5  5  0 | 0  6  6  6  6  0 | 0  7  7  7  7  0
!   0  0  0  0  0  0 | 0  0  0  0  0  0 | 0  0  0  0  0  0 | 0  0  0  0  0  0
!   -----------------|------------------|------------------|-----------------
!   0  0  0  0  0  0 | 0  0  0  0  0  0 | 0  0  0  0  0  0 | 0  0  0  0  0  0
!   0  8  8  8  8  0 | 0  9  9  9  9  0 | 0 10 10 10 10  0 | 0 11 11 11 11  0
!   0  8  8  8  8  0 | 0  9  9  9  9  0 | 0 10 10 10 10  0 | 0 11 11 11 11  0
!   0  8  8  8  8  0 | 0  9  9  9  9  0 | 0 10 10 10 10  0 | 0 11 11 11 11  0
!   0  8  8  8  8  0 | 0  9  9  9  9  0 | 0 10 10 10 10  0 | 0 11 11 11 11  0
!   0  0  0  0  0  0 | 0  0  0  0  0  0 | 0  0  0  0  0  0 | 0  0  0  0  0  0
!   -----------------|------------------|------------------|-----------------
!   0  0  0  0  0  0 | 0  0  0  0  0  0 | 0  0  0  0  0  0 | 0  0  0  0  0  0
!   0 12 12 12 12  0 | 0 13 13 13 13  0 | 0 14 14 14 14  0 | 0 15 15 15 15  0
!   0 12 12 12 12  0 | 0 13 13 13 13  0 | 0 14 14 14 14  0 | 0 15 15 15 15  0
!   0 12 12 12 12  0 | 0 13 13 13 13  0 | 0 14 14 14 14  0 | 0 15 15 15 15  0
!   0 12 12 12 12  0 | 0 13 13 13 13  0 | 0 14 14 14 14  0 | 0 15 15 15 15  0
!   0  0  0  0  0  0 | 0  0  0  0  0  0 | 0  0  0  0  0  0 | 0  0  0  0  0  0
   
  end subroutine init

  subroutine finalize
    use mpi_helper

    implicit none
    deallocate(u,v)
    call MPI_TYPE_FREE(row_type,ierr)
    call MPI_TYPE_FREE(column_type,ierr)
    call MPI_FINALIZE(ierr)
  end subroutine finalize

  subroutine recycle
    use array_bounds
    use mpi_helper
    use filter

    implicit none
    integer (kind=MPI_ADDRESS_KIND)   :: win_size = 0      !< window size
    integer, dimension(0:1)           :: win_get, win_put  !< MPI communication windows

    integer  ::  id_of_pe
    integer  ::  nz=32,ny=6,nbgp=1,nprocs=16
    integer  ::  loop_index, lvl_index, var_index, gp_index 
    integer (kind=MPI_ADDRESS_KIND) :: start_index
    integer  ::  i,j,k

    real, dimension (16,4)  ::  y_1d_pr=0  !< auxiliary variable to calculate avpr, An array that contains cross section profiles in single PE.

    if ( coords(2) /= 0 ) then
       i = nxl
    else
       i = nxlg
    end if

    win_size = 4*(nbgp*nz*ny -2*nbgp)

    call MPI_WIN_CREATE(u(nzb,nys,i), win_size, 4, MPI_INFO_NULL, comm2d, win_get(0), ierr)
    call MPI_WIN_CREATE(v(nzb,nys,i), win_size, 4, MPI_INFO_NULL, comm2d, win_get(1), ierr)

    do k = 0,1
       call MPI_WIN_FENCE(0, win_get(k), ierr)
    end do

!   Vertical slice at the recycling plane is distributed row-wise to all PEs to be contiguous in memory in cross-wind direction
!   Data is distributed not only over the PEs in the vertical slice but over all PEs in order to avoid idle processes while 
!   filtering data in recycling plane. This is an example for a single variable U and a single ghost layer at the inflow.
! 
!==========PE0================PE1================PE2================PE3========= 
!|| 0  0  0  0  0  0 | 0  1  1  1  1  0 | 0  2  2  2  2  0 | 0  3  3  3  3  0 ||  => PE0  0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3
!|| 0  0  0  0  0  0 | 0  1  1  1  1  0 | 0  2  2  2  2  0 | 0  3  3  3  3  0 ||  => PE1  0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 
!|| 0  0  0  0  0  0 | 0  1  1  1  1  0 | 0  2  2  2  2  0 | 0  3  3  3  3  0 ||  => PE2  etc.
!|| 0  0  0  0  0  0 | 0  1  1  1  1  0 | 0  2  2  2  2  0 | 0  3  3  3  3  0 ||  => PE3
!|| 0  0  0  0  0  0 | 0  1  1  1  1  0 | 0  2  2  2  2  0 | 0  3  3  3  3  0 ||  => PE4
!|| 0  0  0  0  0  0 | 0  1  1  1  1  0 | 0  2  2  2  2  0 | 0  3  3  3  3  0 ||  => PE5
!|| 0  0  0  0  0  0 | 0  1  1  1  1  0 | 0  2  2  2  2  0 | 0  3  3  3  3  0 ||  => PE6
!|| 0  0  0  0  0  0 | 0  1  1  1  1  0 | 0  2  2  2  2  0 | 0  3  3  3  3  0 ||  => PE7
!|| 0  0  0  0  0  0 | 0  1  1  1  1  0 | 0  2  2  2  2  0 | 0  3  3  3  3  0 ||  => PE8
!|| 0  0  0  0  0  0 | 0  1  1  1  1  0 | 0  2  2  2  2  0 | 0  3  3  3  3  0 ||  => PE9
!|| 0  0  0  0  0  0 | 0  1  1  1  1  0 | 0  2  2  2  2  0 | 0  3  3  3  3  0 ||  => PE10
!|| 0  0  0  0  0  0 | 0  1  1  1  1  0 | 0  2  2  2  2  0 | 0  3  3  3  3  0 ||  => PE11
!|| 0  0  0  0  0  0 | 0  1  1  1  1  0 | 0  2  2  2  2  0 | 0  3  3  3  3  0 ||  => PE12
!|| 0  0  0  0  0  0 | 0  1  1  1  1  0 | 0  2  2  2  2  0 | 0  3  3  3  3  0 ||  => PE13
!|| 0  0  0  0  0  0 | 0  1  1  1  1  0 | 0  2  2  2  2  0 | 0  3  3  3  3  0 ||  => PE14
!|| 0  0  0  0  0  0 | 0  1  1  1  1  0 | 0  2  2  2  2  0 | 0  3  3  3  3  0 ||  => PE15
!|| 0  0  0  0  0  0 | 0  1  1  1  1  0 | 0  2  2  2  2  0 | 0  3  3  3  3  0 ||  => PE0  copy to 2nd row in PE0 array
!|| 0  0  0  0  0  0 | 0  1  1  1  1  0 | 0  2  2  2  2  0 | 0  3  3  3  3  0 ||  => PE1
!|| 0  0  0  0  0  0 | 0  1  1  1  1  0 | 0  2  2  2  2  0 | 0  3  3  3  3  0 ||  => PE2
!|| 0  0  0  0  0  0 | 0  1  1  1  1  0 | 0  2  2  2  2  0 | 0  3  3  3  3  0 ||  => PE3
!|| 0  0  0  0  0  0 | 0  1  1  1  1  0 | 0  2  2  2  2  0 | 0  3  3  3  3  0 ||  => PE4
!|| 0  0  0  0  0  0 | 0  1  1  1  1  0 | 0  2  2  2  2  0 | 0  3  3  3  3  0 ||  => PE5
!|| 0  0  0  0  0  0 | 0  1  1  1  1  0 | 0  2  2  2  2  0 | 0  3  3  3  3  0 ||  => PE6
!|| 0  0  0  0  0  0 | 0  1  1  1  1  0 | 0  2  2  2  2  0 | 0  3  3  3  3  0 ||  => PE7
!|| 0  0  0  0  0  0 | 0  1  1  1  1  0 | 0  2  2  2  2  0 | 0  3  3  3  3  0 ||  => PE8
!|| 0  0  0  0  0  0 | 0  1  1  1  1  0 | 0  2  2  2  2  0 | 0  3  3  3  3  0 ||  => PE9
!|| 0  0  0  0  0  0 | 0  1  1  1  1  0 | 0  2  2  2  2  0 | 0  3  3  3  3  0 ||  => PE10
!|| 0  0  0  0  0  0 | 0  1  1  1  1  0 | 0  2  2  2  2  0 | 0  3  3  3  3  0 ||  => PE11
!|| 0  0  0  0  0  0 | 0  1  1  1  1  0 | 0  2  2  2  2  0 | 0  3  3  3  3  0 ||  => PE12
!|| 0  0  0  0  0  0 | 0  1  1  1  1  0 | 0  2  2  2  2  0 | 0  3  3  3  3  0 ||  => PE13
!|| 0  0  0  0  0  0 | 0  1  1  1  1  0 | 0  2  2  2  2  0 | 0  3  3  3  3  0 ||  => PE14
!|| 0  0  0  0  0  0 | 0  1  1  1  1  0 | 0  2  2  2  2  0 | 0  3  3  3  3  0 ||  => PE15

    loop_index = my_rank ! indicates row index in the global grid
    lvl_index = 1 ! indicates row index in array with horizontal 1D data

    do while (loop_index < nz*nbgp*2) ! number of rows is number of vertical rows nz * thickness of the ghost layer at inflow * number of vars that are recycled (2 in this case U,V)

       var_index = loop_index/(nz*nbgp) ! variable index u=0, v=1
       gp_index  = loop_index/nz-var_index*nbgp ! indicate index of ghost point 0,1,2
       start_index = mod(loop_index,nz) + gp_index*nz*ny ! memory shift to start index of y-dimension row

       do j = 0,dims(2)-1
          call MPI_CART_RANK(comm2d, (/j,1/), id_of_pe, ierr)
          call MPI_GET(y_1d_pr(j*(ny-2)+1,lvl_index), ny-2, MPI_REAL, id_of_pe, start_index, 1, row_type, win_get(var_index), ierr)
       end do

       loop_index = loop_index + nprocs
       lvl_index = lvl_index+1
    end do

    do k = 0,1
       call MPI_WIN_FENCE(0, win_get(k), ierr)
    end do

    do k = 0,1
      call MPI_WIN_FREE(win_get(k), ierr)
    end do

    ! 1D-filter operation
    call filter1d(y_1d_pr)
    ! put filtered data back into ghost layer of inflow plane (like above with replacing get by put)

    call MPI_WIN_CREATE(u(nzb,nys,i), win_size, 4, MPI_INFO_NULL, comm2d, win_put(0), ierr)
    call MPI_WIN_CREATE(v(nzb,nys,i), win_size, 4, MPI_INFO_NULL, comm2d, win_put(1), ierr)

    do k = 0,1
       call MPI_WIN_FENCE(0, win_put(k), ierr)
    end do

    loop_index = my_rank ! indicates row index in the global grid
    lvl_index = 1 ! indicates row index in array with horizontal 1D data

    do while (loop_index < nz*nbgp*2) ! number of rows is number of vertical rows nz * thickness of the ghost layer at inflow * number of vars that are recycled (2 in this case U,V)

       var_index = loop_index/(nz*nbgp) ! variable index u=0, v=1
       gp_index  = loop_index/nz-var_index*nbgp ! indicate index of ghost point 0,1,2
       start_index = mod(loop_index,nz) + gp_index*nz*ny ! memory shift to start index of y-dimension row 

       do j = 0,dims(2)-1
          call MPI_CART_RANK(comm2d, (/j,0/), id_of_pe, ierr)
          call MPI_PUT(y_1d_pr(j*(ny-2)+1,lvl_index), ny-2, MPI_REAL, id_of_pe, start_index, 1, row_type, win_put(var_index), ierr)
       end do

       loop_index = loop_index + nprocs
       lvl_index = lvl_index+1
    end do

    do k = 0,1
       call MPI_WIN_FENCE(0, win_put(k), ierr)
    end do

    do k = 0,1
      call MPI_WIN_FREE(win_put(k), ierr)
    end do

  end subroutine recycle

end program rma_recycle
