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

    ! file creation
    PROCEDURE, PRIVATE :: create_serial => nc_create_serial
#if defined( parallel )
    PROCEDURE, PRIVATE :: create_par => nc_create_par
    GENERIC, PUBLIC :: nc_create => create_serial, create_par
#else
    GENERIC, PUBLIC :: nc_create => create_serial
#endif

    ! definitions
    PROCEDURE nc_defdim
    PROCEDURE nc_defvar
    PROCEDURE nc_enddef

    ! Attributes
    PROCEDURE, PRIVATE :: var_put_str_att => nc_var_put_str_att
    PROCEDURE, PRIVATE :: var_put_int_att => nc_var_put_int_att
    PROCEDURE, PRIVATE :: var_put_real_att => nc_var_put_real_att
    PROCEDURE, PRIVATE :: glo_put_str_att => nc_glo_put_str_att
    PROCEDURE, PRIVATE :: glo_put_int_att => nc_glo_put_int_att
    PROCEDURE, PRIVATE :: glo_put_real_att => nc_glo_put_real_att
    GENERIC, PUBLIC :: nc_put_att => var_put_str_att, var_put_int_att, var_put_real_att, &
                                     glo_put_str_att, glo_put_int_att, glo_put_real_att

    ! var access
#if defined( parallel )
    PROCEDURE nc_var_par_access
#endif

    ! file opening
    PROCEDURE, PRIVATE :: open_serial => nc_open_serial
#if defined( parallel )
    PROCEDURE, PRIVATE :: open_par => nc_open_par
    GENERIC, PUBLIC :: nc_open => open_serial, open_par
#else
    GENERIC, PUBLIC :: nc_open => open_serial
#endif

    ! file closing
    PROCEDURE nc_close

    ! variable read
    PROCEDURE, PRIVATE :: read1D => nc_read1D
    PROCEDURE, PRIVATE :: read2D => nc_read2D
    PROCEDURE, PRIVATE :: read3D => nc_read3D
    GENERIC, PUBLIC :: nc_read => read1D, read2D, read3D

    ! variable write
    PROCEDURE, PRIVATE :: write1D => nc_write1D
    PROCEDURE, PRIVATE :: write2D => nc_write2D
    GENERIC,PUBLIC :: nc_write => write1D, write2D

    FINAL :: destructor !requires full Fortran2003 support by compiler
  END TYPE

CONTAINS

  SUBROUTINE destructor(this)
    ! close open nc files before object gets destroyed
    IMPLICIT NONE
    TYPE(nc_file) :: this
    INTEGER :: nc_stat

    nc_stat = nf90_close(this%ncid)
  END SUBROUTINE destructor
 
  INTEGER FUNCTION nc_create_serial(this, file_name)
    ! Create new netcdf-file and overwrite in case file already exists
    ! input value: file_name(char*) = name of the file
    ! return value: netcdf error code
    IMPLICIT NONE
    CLASS(nc_file) :: this
    CHARACTER (len = *) :: file_name

    nc_create_serial = nf90_create(file_name, NF90_CLOBBER, this%ncid)
    CALL nc_handle_err(nc_create_serial)
  END FUNCTION nc_create_serial

#if defined( parallel )
  INTEGER FUNCTION nc_create_par(this, file_name, cmode, mpi_comm, mpi_info)
    ! Create new parallel netcdf-file and overwrite in case file already exists
    ! input value: file_name(char*) = name of the file
    ! return value: netcdf error code
    IMPLICIT NONE
    CLASS(nc_file) :: this
    CHARACTER (len = *) :: file_name
    INTEGER, INTENT(in) :: cmode
    INTEGER, INTENT(in) :: mpi_comm
    INTEGER, INTENT(in) :: mpi_info

    nc_create_par = nf90_create_par(file_name, cmode, mpi_comm, mpi_info, this%ncid)
    CALL nc_handle_err(nc_create_par)
  END FUNCTION nc_create_par
#endif

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
    CALL nc_handle_err(nc_defdim)
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
    CALL nc_handle_err(nc_defvar)
  END FUNCTION nc_defvar

  INTEGER FUNCTION nc_var_put_str_att(this, var_name, att_name, att_value)
  ! Add an string attribute to a variable
  ! input values: var_name(char*) = name of the variable
  !               att_name (char*) = attribute name
  !               values (char*) = attribute value
  IMPLICIT NONE
  CLASS(nc_file) :: this
  CHARACTER (len = *) :: var_name
  CHARACTER (len = *) :: att_name
  CHARACTER (len = *) :: att_value
  INTEGER :: var_id

  nc_var_put_str_att = nf90_inq_varid(this%ncid, var_name, var_id)
  CALL nc_handle_err(nc_var_put_str_att)
  nc_var_put_str_att = nf90_put_att(this%ncid, var_id, att_name, att_value)
  CALL nc_handle_err(nc_var_put_str_att)
  END FUNCTION

  INTEGER FUNCTION nc_var_put_int_att(this, var_name, att_name, att_value)
  ! Add an integer attribute to a variable
  ! input values: var_name(char*) = name of the variable
  !               att_name (char*) = attribute name
  !               values (int) = attribute value
  IMPLICIT NONE
  CLASS(nc_file) :: this
  CHARACTER (len = *) :: var_name
  CHARACTER (len = *) :: att_name
  INTEGER,DIMENSION(:) :: att_value
  INTEGER :: var_id

  nc_var_put_int_att = nf90_inq_varid(this%ncid, var_name, var_id)
  CALL nc_handle_err(nc_var_put_int_att)
  nc_var_put_int_att = nf90_put_att(this%ncid, var_id, att_name, att_value)
  CALL nc_handle_err(nc_var_put_int_att)
  END FUNCTION

  INTEGER FUNCTION nc_var_put_real_att(this, var_name, att_name, att_value)
  ! Add a real attribute to a variable
  ! input values: var_name(char*) = name of the variable
  !               att_name (char*) = attribute name
  !               values (real) = attribute value
  IMPLICIT NONE
  CLASS(nc_file) :: this
  CHARACTER (len = *) :: var_name
  CHARACTER (len = *) :: att_name
  REAL,DIMENSION(:) :: att_value
  INTEGER :: var_id

  nc_var_put_real_att = nf90_inq_varid(this%ncid, var_name, var_id)
  CALL nc_handle_err(nc_var_put_real_att)
  nc_var_put_real_att = nf90_put_att(this%ncid, var_id, att_name, att_value)
  CALL nc_handle_err(nc_var_put_real_att)
  END FUNCTION

  INTEGER FUNCTION nc_glo_put_str_att(this, att_name, att_value)
  ! Add an string attribute to global
  ! input values: att_name (char*) = attribute name
  !               values (char*) = attribute value
  IMPLICIT NONE
  CLASS(nc_file) :: this
  CHARACTER (len = *) :: att_name
  CHARACTER (len = *) :: att_value
  INTEGER :: var_id

  nc_glo_put_str_att = nf90_put_att(this%ncid, NF90_GLOBAL, att_name, att_value)
  CALL nc_handle_err(nc_glo_put_str_att)
  END FUNCTION

  INTEGER FUNCTION nc_glo_put_int_att(this, att_name, att_value)
  ! Add an integer attribute to global
  ! input values: att_name (char*) = attribute name
  !               values (char*) = attribute value
  IMPLICIT NONE
  CLASS(nc_file) :: this
  CHARACTER (len = *) :: att_name
  INTEGER, DIMENSION(:) :: att_value
  INTEGER :: var_id

  nc_glo_put_int_att = nf90_put_att(this%ncid, NF90_GLOBAL, att_name, att_value)
  CALL nc_handle_err(nc_glo_put_int_att)
  END FUNCTION

  INTEGER FUNCTION nc_glo_put_real_att(this, att_name, att_value)
  ! Add a real attribute to global
  ! input values: att_name (char*) = attribute name
  !               values (char*) = attribute value
  IMPLICIT NONE
  CLASS(nc_file) :: this
  CHARACTER (len = *) :: att_name
  REAL, DIMENSION(:) :: att_value
  INTEGER :: var_id

  nc_glo_put_real_att = nf90_put_att(this%ncid, NF90_GLOBAL, att_name, att_value)
  CALL nc_handle_err(nc_glo_put_real_att)
  END FUNCTION

  INTEGER FUNCTION nc_enddef(this)
    ! End definitions, leave define mode
    ! return value: netcdf error code
    IMPLICIT NONE
    CLASS(nc_file) :: this

    nc_enddef = nf90_enddef(this%ncid)
    CALL nc_handle_err(nc_enddef)
  END FUNCTION nc_enddef

  INTEGER FUNCTION nc_open_serial(this,file_name)
    ! Open existing netcdf-file in read-only mode
    ! input value: file_name (char*) = name of file to be opened
    ! return value: netcdf error code
    IMPLICIT NONE
    CLASS(nc_file) :: this
    CHARACTER (len = *) :: file_name

    nc_open_serial = nf90_open(file_name, NF90_NOWRITE, this%ncid)
    CALL nc_handle_err(nc_open_serial)
  END FUNCTION nc_open_serial

#if defined( parallel )
  INTEGER FUNCTION nc_open_par(this, file_name, cmode, mpi_comm, mpi_info)
    ! Open existing parallel netcdf-file
    ! input value: file_name (char*) = name of file to be opened
    ! return value: netcdf error code
    IMPLICIT NONE
    CLASS(nc_file) :: this
    CHARACTER (len = *) :: file_name
    INTEGER, INTENT(in) :: cmode
    INTEGER, INTENT(in) :: mpi_comm
    INTEGER, INTENT(in) :: mpi_info

    nc_open_par = nf90_open_par(file_name, cmode, mpi_comm, &
              mpi_info, this%ncid)
    CALL nc_handle_err(nc_open_par)
  END FUNCTION nc_open_par

  INTEGER FUNCTION nc_var_par_access(this, var_name, par_access)
    ! The function NF90_VAR_PAR_ACCESS changes whether read/write operations 
    ! on a parallel file system are performed collectively (the default) or independently on the variable
    IMPLICIT NONE
    CLASS(nc_file) :: this
    CHARACTER (len = *) :: var_name
    INTEGER :: var_id
    INTEGER, INTENT(in) :: par_access
    INTEGER :: nc_stat

    nc_stat = nf90_inq_varid(this%ncid, var_name, var_id)
    CALL nc_handle_err(nc_stat)
    nc_var_par_access = nf90_var_par_access(this%ncid, var_id, par_access)
    CALL nc_handle_err(nc_var_par_access)
  END FUNCTION nc_var_par_access
#endif

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
    CALL nc_handle_err(nc_stat)
    nc_stat = nf90_get_var(this%ncid, var_id, data_out, start, count, stride, map)
    CALL nc_handle_err(nc_stat)
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
    CALL nc_handle_err(nc_stat)
    nc_stat = nf90_get_var(this%ncid, var_id, data_out, start, count, stride, map)
    CALL nc_handle_err(nc_stat)
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
    CALL nc_handle_err(nc_stat)
    nc_stat = nf90_get_var(this%ncid, var_id, data_out, start, count, stride, map)
    CALL nc_handle_err(nc_stat)
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
    CALL nc_handle_err(nc_stat)
    nc_stat = nf90_put_var(this%ncid, var_id, data_in, start, count, stride, map)
    CALL nc_handle_err(nc_stat)
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
    CALL nc_handle_err(nc_stat)
    nc_stat = nf90_put_var(this%ncid, var_id, data_in, start, count, stride, map)
    CALL nc_handle_err(nc_stat)
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
    CALL nc_handle_err(nc_stat)
    nc_stat = nf90_put_var(this%ncid, var_id, data_in, start, count, stride, map)
    CALL nc_handle_err(nc_stat)
  END SUBROUTINE nc_write3D


  INTEGER FUNCTION nc_close(this)
  ! Close the file, freeing all resources
    IMPLICIT NONE
    CLASS(nc_file) :: this

    nc_close = nf90_close(this%ncid)
    CALL nc_handle_err(nc_close)
  END FUNCTION nc_close

  SUBROUTINE nc_handle_err(errcode)
  ! Output netcdf error message
  IMPLICIT NONE
    INTEGER, INTENT(in) :: errcode
    
    IF(errcode /= nf90_noerr) THEN
       PRINT *, 'Error: ', TRIM(nf90_strerror(errcode))
       STOP 2
    ENDIF
  END SUBROUTINE nc_handle_err

END MODULE netcdf_handle
