program rowtype_test

  use mpi

  implicit none
  integer                ::  my_rank, ierr
  integer,dimension(5,5) ::  matrix
  integer,dimension(3,3) ::  inner_matrix
  integer                ::  i,j,k

  integer                ::  win, rowtype 
  integer (KIND=MPI_ADDRESS_KIND) ::  sizeofint=4, start_index 

  call MPI_Init(ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierr)

  k = 0
  do i = 1,5
    do j = 1,5
      if (i > 1 .and. i < 5 .and. j > 1 .and. j < 5) then
        k = k+1
        matrix(j,i) = k
      else
        matrix(j,i) = 0
      end if
    end do
  end do
!----------------------------------------------------------------------------------------------------------------------------
!  Create a 2D 5X5 array with one layer of ghost points
!  looks like this
!  0 0 0 0 0
!  0 1 4 7 0
!  0 2 5 8 0
!  0 3 6 9 0
!  0 0 0 0 0

!  print to std output
  if (my_rank == 0) then
    write(*,*) "matrix on rank 0"
    do j=1,5
      write(*,*) matrix(j,:)
    end do
  end if 

!  How it looks in memory
! i=1       i=2       i=3       i=4       i=5
!  |         |         |         |         |
!  v         v         v         v         v
!  0 0 0 0 0 0 1 2 3 0 0 4 5 6 0 0 7 8 9 0 0 0 0 0 0
!  ^         ^         ^
!  |         |         |
!  Block 1   Block 2   Block 3   
!  Create a row typ that take three blocks of size 1 with stride 5 from memory
  CALL MPI_TYPE_VECTOR(3,1,5,MPI_INT,rowtype,ierr)
  CALL MPI_TYPE_COMMIT(rowtype,ierr)
!----------------------------------------------------------------------------------------------------------------------------
!  Create a memory access window to the data starting from reference point i=2,j=2 and length of 13*4 byte with 4 byte stride

!       beginning of window      end of window
!              |                       |
!              v                       v
!  0 0 0 0 0 0 1 2 3 0 0 4 5 6 0 0 7 8 9 0 0 0 0 0 0
  CALL MPI_WIN_CREATE(matrix(2,2), 13*sizeofint, sizeofint, MPI_INFO_NULL, MPI_COMM_WORLD, win, ierr)

!----------------------------------------------------------------------------------------------------------------------------
!  Start a communication epoch
  CALL MPI_WIN_FENCE(0, win, ierr)
 
! Rank 1 Fetches inner data row-wise from matrix in rank 0 and writes data column-wise into inner matrix in rank 1 so result should be transposed inner matrix
! -> 1 2 3
!    4 5 6
!    7 8 9

  do i=1,3
    start_index=i-1
    if (my_rank == 1) then

!  First iteration, i=1
!  |         |         |
!  v         v         v
!  1 2 3 0 0 4 5 6 0 0 7 8 9

!  Second iteration, i=2
!    |         |         |
!    v         v         v
!  1 2 3 0 0 4 5 6 0 0 7 8 9

!  Third iteration, i=3
!      |         |         |
!      v         v         v
!  1 2 3 0 0 4 5 6 0 0 7 8 9

      CALL MPI_GET(inner_matrix(1,i), 3, MPI_INT, int(0), start_index, 1, rowtype, win, ierr)
    end if
  end do
!  End communication epoch
  CALL MPI_WIN_FENCE(0,win, ierr)
  CALL MPI_WIN_FREE(win, ierr)
  CALL MPI_TYPE_FREE(rowtype, ierr)

! Print matrix in rank 0 and inner matrix in rank1
  if (my_rank == 1) then
    write(*,*) "inner matrix send to and transposed on rank 1"
    do j=1,3
      write(*,*) inner_matrix(j,:)
    end do
  end if

  CALL MPI_FINALIZE(ierr)

end program rowtype_test
