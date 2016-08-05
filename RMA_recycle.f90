module array_bounds
  implicit none
  integer  ::  nxlg, nxl, nxrg, nxr, nysg, nys, nyng, nyn, nzb, nzt
end module array_bounds

module mpi_helper
  implicit none
  integer  ::  my_rank, ierr, comm2d, row_type, column_type
  integer, dimension(2)      ::  dims, coords
end module mpi_helper

program rma_recycle

  use mpi
  use mpi_helper
  use array_bounds

  implicit none
  real, allocatable, dimension(:,:,:)    ::  u,v
 
  call init

  call recycle

  call final

  contains

  subroutine init
    use array_bounds
    use mpi_helper

    implicit none
    call MPI_Init(ierr)

    dims(1) = 4
    dims(2) = 4

    call MPI_Cart_Create(mpi_comm_world, 2, dims, (/ .false. , .false. /), .true., comm2d, ierr)
    call MPI_Comm_rank(comm2d, my_rank, ierr)
    call MPI_Cart_Coords(comm2d, my_rank, 2, coords, ierr)

    call MPI_TYPE_VECTOR(4,1,6,MPI_INT,row_type,ierr)
    call MPI_TYPE_COMMIT(row_type,ierr)

    call MPI_TYPE_VECTOR(1,4,6,MPI_INT,column_type,ierr)
    call MPI_TYPE_COMMIT(column_type,ierr)


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
    write(*,*) "Rank: ", my_rank, " Size: ", size(u), (/nxlg,nxrg,nysg,nyng/)

    u(:,nys:nyn,nxl:nxr) = my_rank;
    v = 2*u;
  end subroutine init

  subroutine final
    use mpi_helper

    implicit none
    deallocate(u,v)
    call MPI_TYPE_FREE(row_type,ierr)
    call MPI_TYPE_FREE(column_type,ierr)
    call MPI_FINALIZE(ierr)
  end subroutine final

  subroutine recycle
    use array_bounds
    use mpi_helper

    implicit none
    INTEGER (KIND=MPI_ADDRESS_KIND) :: win_size = 0      !< window size
    INTEGER, DIMENSION(0:1)           :: win_get, win_put  !< MPI communication windows

    integer  ::  id_of_pe
    integer  ::  nz=32,ny=6,nbgp=1,nprocs=16
    integer  ::  loop_index, lvl_index, var_index, gp_index 
    integer (kind=MPI_ADDRESS_KIND) :: start_index
    integer  ::  i,j

    REAL, DIMENSION (16,4)  ::  y_1d_pr=0, y_1d_pr_filter  !< auxiliary variable to calculate avpr

    if ( coords(2) == 1 ) then
       i = nxl
    else
       i = nxlg
    end if

    IF ( coords(2) == 1 )  THEN
       win_size = 4*(nbgp*nz*ny -2*nbgp)
    END IF

    CALL MPI_WIN_CREATE(u(nzb,nys,i), win_size, 4, MPI_INFO_NULL, comm2d, win_get(0), ierr)
    CALL MPI_WIN_CREATE(v(nzb,nys,i), win_size, 4, MPI_INFO_NULL, comm2d, win_get(1), ierr)

    DO i = 0,1
       CALL MPI_WIN_FENCE(0, win_get(i), ierr)
    END DO

    loop_index = my_rank
    lvl_index = 1

    DO WHILE (loop_index < nz*nbgp*2)

       var_index = loop_index/(nz*nbgp) ! variable index u=0, v=1
       gp_index  = loop_index/(nz*(var_index+1)) ! indicate index of ghost point 0,1,2
       start_index = mod(loop_index-var_index*nz,nz) + gp_index*ny*nz ! memory shift to start index of y-dimension row TODO check this is correct

       DO j = 0,dims(2)-1
          CALL MPI_CART_RANK(comm2d, (/j,1/), id_of_pe, ierr)
          CALL MPI_GET(y_1d_pr(j*(ny-2)+1,lvl_index), ny-2, MPI_REAL, id_of_pe, start_index, 1, row_type, win_get(var_index), ierr)
       END DO

       loop_index = loop_index + nprocs
       lvl_index = lvl_index+1
    END DO

    DO i = 0,1
       CALL MPI_WIN_FENCE(0, win_get(i), ierr)
    END DO

    DO i = 0,1
      CALL MPI_WIN_FREE(win_get(i), ierr)
    END DO

    ! output to see if values are correctly transferred from recycling plane to 1D-filter arrays
    write(*,*) "Rank: ", my_rank, " Array: ", y_1d_pr

    !TODO 
    ! - 1D-filter operation
    ! - put filtered data back into ghost layer of inflow plane (like above with replacing get by put)

  end subroutine recycle

end program rma_recycle
