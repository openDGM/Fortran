! This is a netCDF wrapper for fortran 2003
! Date: Jan 2016
! Author: Andreas Tack
!
! Provides file handle to netCDF file and procedures to manipulate

MODULE netcdf_handle
  USE netcdf
  IMPLICIT NONE

  TYPE nc_file
    INTEGER :: ncid

  CONTAINS
    PROCEDURE nc_create
    PROCEDURE nc_defdim
    PROCEDURE nc_defvar
    PROCEDURE nc_enddef
    PROCEDURE nc_open
    PROCEDURE nc_close
    PROCEDURE, PRIVATE :: read1D => nc_read1D
    PROCEDURE, PRIVATE :: read2D => nc_read2D
    PROCEDURE, PRIVATE :: read3D => nc_read3D
    GENERIC, PUBLIC :: nc_read => read1D, read2D, read3D
    PROCEDURE, PRIVATE :: write1D => nc_write1D
    PROCEDURE, PRIVATE :: write2D => nc_write2D
    GENERIC,PUBLIC :: nc_write => write1D, write2D

!    FINAL :: destructor !requires full Fortran2003 support by compiler
  END TYPE

CONTAINS

!  SUBROUTINE destructor(this)
!    ! close open nc files before object gets destroyed
!    IMPLICIT NONE
!    TYPE(nc_file) :: this
!    INTEGER :: nc_stat
!
!    nc_stat = nf90_close(this%ncid)
!  END SUBROUTINE destructor
 
  INTEGER FUNCTION nc_create(this, file_name)
    ! Create new netcdf-file and overwrite in case file already exists
    ! input value: file_name(char*) = name of the file
    ! return value: netcdf error code
    IMPLICIT NONE
    CLASS(nc_file) :: this
    CHARACTER (len = *) :: file_name

    nc_create = nf90_create(file_name, NF90_CLOBBER, this%ncid)
  END FUNCTION nc_create

  INTEGER FUNCTION nc_defdim(this, dim_name, dim_size)
    ! Define new dimension
    ! input values: dim_name(char*) = name of the dimension
    !               dim_size(int) = size of the dimension 
    ! return value: netcdf error code
    IMPLICIT NONE
    CLASS(nc_file) :: this
    CHARACTER (len = *) :: dim_name
    INTEGER :: dim_size, dim_id

    nc_defdim = nf90_def_dim(this%ncid,dim_name, dim_size, dim_id)
  END FUNCTION nc_defdim

  INTEGER FUNCTION nc_defvar(this, var_name, NCTYPE, dim_ids)
    ! Define new variable
    ! input values: var_name(char*) = name of the variable
    !               NCTYPE = netcdf-type of the variable (e.g. NF90_INT)
    !               dim_ids (int(:)) = array containing dimension id's bound to newly defined variable
    ! return value: netcdf error code
    IMPLICIT NONE
    CLASS(nc_file) :: this
    CHARACTER (len = *) :: var_name
    INTEGER :: NCTYPE, var_id
    INTEGER, DIMENSION(:) :: dim_ids

    nc_defvar = nf90_def_var(this%ncid, var_name, NCTYPE, dim_ids, var_id)
  END FUNCTION nc_defvar

  INTEGER FUNCTION nc_enddef(this)
    ! End definitions, leave define mode
    ! return value: netcdf error code
    IMPLICIT NONE
    CLASS(nc_file) :: this

    nc_enddef = nf90_enddef(this%ncid)
  END FUNCTION nc_enddef

  INTEGER FUNCTION nc_open(this,file_name)
    ! Open existing netcdf-file in read-only mode
    ! input value: file_name (char*) = name of file to be opened
    ! return value: netcdf error code
    IMPLICIT NONE
    CLASS(nc_file) :: this
    CHARACTER (len = *) :: file_name

    nc_open = nf90_open(file_name, NF90_NOWRITE, this%ncid)
  END FUNCTION nc_open

  SUBROUTINE nc_read1D(this, var_name, data_out, nc_stat, start, count, stride, map)
    ! Read variable data from file
    ! input values: var_name (char*) = name of variable
    !               data_out (real(:)) = the array containing data of the variable
    !               nc_stat (int) = netcdf error code
    IMPLICIT NONE
    CLASS(nc_file) :: this
    CHARACTER (len = *) :: var_name
    !TODO Figure out some template like way to allow different type and dimension for data
    REAL, DIMENSION(:) :: data_out
    INTEGER, INTENT(out) :: nc_stat
    INTEGER :: var_id
    INTEGER, OPTIONAL, DIMENSION(:) :: start, count, stride, map

    ! Get the varid of the data variable, based on its name.
    nc_stat = nf90_inq_varid(this%ncid, var_name, var_id)
    nc_stat = nf90_get_var(this%ncid, var_id, data_out, start, count, stride, map)
  END SUBROUTINE nc_read1D

  SUBROUTINE nc_read2D(this, var_name, data_out, nc_stat, start, count, stride, map)
    ! Read variable data from file
    ! input values: var_name (char*) = name of variable
    !               data (real(:,:)) = the array containing data of the variable
    !               nc_stat (int) = netcdf error code
    IMPLICIT NONE
    CLASS(nc_file) :: this
    CHARACTER (len = *) :: var_name
    !TODO Figure out some template like way to allow generic type for data
    REAL, DIMENSION(:,:) :: data_out
    INTEGER, INTENT(out) :: nc_stat
    INTEGER :: var_id
    INTEGER, OPTIONAL, DIMENSION(:) :: start, count, stride, map

    ! Get the varid of the data variable, based on its name.
    nc_stat = nf90_inq_varid(this%ncid, var_name, var_id)
    nc_stat = nf90_get_var(this%ncid, var_id, data_out, start, count, stride, map)
  END SUBROUTINE nc_read2D

  SUBROUTINE nc_read3D(this, var_name, data_out, nc_stat, start, count, stride, map)
    ! Read variable data from file
    ! input values: var_name (char*) = name of variable
    !               data (real(:,:,:)) = the array containing data of the variable
    !               nc_stat (int) = netcdf error code
    IMPLICIT NONE
    CLASS(nc_file) :: this
    CHARACTER (len = *) :: var_name
    !TODO Figure out some template like way to allow generic type for data
    REAL, DIMENSION(:,:,:) :: data_out
    INTEGER, INTENT(out) :: nc_stat
    INTEGER :: var_id
    INTEGER, OPTIONAL, DIMENSION(:) :: start, count, stride, map

    ! Get the varid of the data variable, based on its name.
    nc_stat = nf90_inq_varid(this%ncid, var_name, var_id)
    nc_stat = nf90_get_var(this%ncid, var_id, data_out, start, count, stride, map)

  END SUBROUTINE nc_read3D


  SUBROUTINE nc_write1D(this, var_name, data_in, nc_stat, start, count, stride, map)
    ! Write variable data to file
    ! input values: var_name (char*) = name of variable
    !               data_in (real(:)) = the array containing data of the variable
    !               nc_stat (int) = netcdf error code
    IMPLICIT NONE
    CLASS(nc_file) :: this
    CHARACTER (len = *) :: var_name
    !TODO Figure out some template like way to allow generic type for data
    REAL, DIMENSION(:) :: data_in
    INTEGER, INTENT(out) :: nc_stat
    INTEGER :: var_id
    INTEGER, OPTIONAL, DIMENSION(:) :: start, count, stride, map


    ! Get the varid of the data variable, based on its name.
    nc_stat = nf90_inq_varid(this%ncid, var_name, var_id)
    nc_stat = nf90_put_var(this%ncid, var_id, data_in, start, count, stride, map)
  END SUBROUTINE nc_write1D

  SUBROUTINE nc_write2D(this, var_name, data_in, nc_stat, start, count, stride, map)
    ! Write variable data to file
    ! input values: var_name (char*) = name of variable
    !               data (real(:,:)) = the array containing data of the variable
    !               nc_stat (int) = netcdf error code
    IMPLICIT NONE
    CLASS(nc_file) :: this
    CHARACTER (len = *) :: var_name

    !TODO Figure out some template like way to allow generic type for data
    REAL, DIMENSION(:,:) :: data_in
    INTEGER, INTENT(out) :: nc_stat
    INTEGER :: var_id
    INTEGER, OPTIONAL, DIMENSION(:) :: start, count, stride, map

    ! Get the varid of the data variable, based on its name.
    nc_stat = nf90_inq_varid(this%ncid, var_name, var_id)
    nc_stat = nf90_put_var(this%ncid, var_id, data_in, start, count, stride, map)
  END SUBROUTINE nc_write2D

  SUBROUTINE nc_write3D(this, var_name, data_in, nc_stat, start, count, stride, map)
    ! Write variable data to file
    ! input values: var_name (char*) = name of variable
    !               data (real(:,:,:)) = the array containing data of the variable
    !               nc_stat (int) = netcdf error code
    IMPLICIT NONE
    CLASS(nc_file) :: this
    CHARACTER (len = *) :: var_name

    !TODO Figure out some template like way to allow generic type for data
    REAL, DIMENSION(:,:,:) :: data_in
    INTEGER, INTENT(out) :: nc_stat
    INTEGER :: var_id
    INTEGER, OPTIONAL, DIMENSION(:) :: start, count, stride, map

    ! Get the varid of the data variable, based on its name.
    nc_stat = nf90_inq_varid(this%ncid, var_name, var_id)
    nc_stat = nf90_put_var(this%ncid, var_id, data_in, start, count, stride, map)
  END SUBROUTINE nc_write3D


  INTEGER FUNCTION nc_close(this)
  ! Close the file, freeing all resources
    IMPLICIT NONE
    CLASS(nc_file) :: this

    nc_close = nf90_close(this%ncid)
  END FUNCTION nc_close

END MODULE netcdf_handle
