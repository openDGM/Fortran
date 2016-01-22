! This is a netCDF wrapper for fortran 2003
! Date: Jan 2016
! Author: Andreas Tack
!
! Provides file handle to netCDF file and procedures to manipulate

module netcdf_handle
  use netcdf
  implicit none

  type nc_file
    integer :: ncid

  contains
    procedure nc_create
    procedure nc_defdim
    procedure nc_defvar
    procedure nc_enddef
    procedure nc_open
    procedure nc_close
    procedure, private :: read1D => nc_read1D
    procedure, private :: read2D => nc_read2D
    generic, public :: nc_read => read1D, read2D
    procedure, private :: write1D => nc_write1D
    procedure, private :: write2D => nc_write2D
    generic,public :: nc_write => write1D, write2D
  end type

contains
  
  integer function nc_create(this, file_name)
    ! Create new netcdf-file and overwrite in case file already exists
    ! input value: file_name(char*) = name of the file
    ! return value: netcdf error code
    implicit none
    class(nc_file) :: this
    character (len = *) :: file_name

    nc_create = nf90_create(file_name, NF90_CLOBBER, this%ncid)
  end function nc_create

  integer function nc_defdim(this, dim_name, dim_size)
    ! Define new dimension
    ! input values: dim_name(char*) = name of the dimension
    !               dim_size(int) = size of the dimension 
    ! return value: netcdf error code
    implicit none
    class(nc_file) :: this
    character (len = *) :: dim_name
    integer :: dim_size, dim_id

    nc_defdim = nf90_def_dim(this%ncid,dim_name, dim_size, dim_id)
  end function nc_defdim

  integer function nc_defvar(this, var_name, NCTYPE, dim_ids)
    ! Define new variable
    ! input values: var_name(char*) = name of the variable
    !               NCTYPE = netcdf-type of the variable (e.g. NF90_INT)
    !               dim_ids (int(:)) = array containing dimension id's bound to newly defined variable
    ! return value: netcdf error code
    implicit none
    class(nc_file) :: this
    character (len = *) :: var_name
    integer :: NCTYPE, var_id
    integer, dimension(:) :: dim_ids

    nc_defvar = nf90_def_var(this%ncid, var_name, NCTYPE, dim_ids, var_id)
  end function nc_defvar

  integer function nc_enddef(this)
    ! End definitions, leave define mode
    ! return value: netcdf error code
    implicit none
    class(nc_file) :: this

    nc_enddef = nf90_enddef(this%ncid)
  end function nc_enddef


  integer function nc_open(this,file_name)
    ! Open existing netcdf-file in read-only mode
    ! input value: file_name (char*) = name of file to be opened
    ! return value: netcdf error code
    implicit none
    class(nc_file) :: this
    character (len = *) :: file_name

    nc_open = nf90_open(file_name, NF90_NOWRITE, this%ncid)
  end function nc_open

  subroutine nc_read1D(this, var_name, data, stat, start, count, stride, map)
    ! Read variable data from file
    ! input values: var_name (char*) = name of variable
    !               data (real(:,:)) = the array containing data of the variable
    !               stat (int) = netcdf error code
    implicit none
    class(nc_file) :: this
    character (len = *) :: var_name
    !TODO Figure out some template like way to allow different type and dimension for data
    real, dimension(:) :: data
    integer, intent(out) :: stat
    integer :: var_id
    integer, optional, dimension(:) :: start, count, stride, map

    ! Get the varid of the data variable, based on its name.
    stat = nf90_inq_varid(this%ncid, var_name, var_id)
    stat = nf90_get_var(this%ncid, var_id, data, start, count, stride, map)
  end subroutine nc_read1D

  subroutine nc_read2D(this, var_name, data, stat, start, count, stride, map)
    ! Read variable data from file
    ! input values: var_name (char*) = name of variable
    !               data (real(:,:)) = the array containing data of the variable
    !               stat (int) = netcdf error code
    implicit none
    class(nc_file) :: this
    character (len = *) :: var_name
    !TODO Figure out some template like way to allow generic type for data
    real, dimension(:,:) :: data
    integer, intent(out) :: stat
    integer :: var_id
    integer, optional, dimension(:) :: start, count, stride, map

    ! Get the varid of the data variable, based on its name.
    stat = nf90_inq_varid(this%ncid, var_name, var_id)
    stat = nf90_get_var(this%ncid, var_id, data, start, count, stride, map)
  end subroutine nc_read2D

  subroutine nc_write1D(this, var_name, data, stat, start, count, stride, map)
    ! Write variable data to file
    ! input values: var_name (char*) = name of variable
    !               data (real(:,:)) = the array containing data of the variable
    !               stat (int) = netcdf error code
    implicit none
    class(nc_file) :: this
    character (len = *) :: var_name
    !TODO Figure out some template like way to allow generic type for data
    real, dimension(:) :: data
    integer, intent(out) :: stat
    integer :: var_id
    integer, optional, dimension(:) :: start, count, stride, map


    ! Get the varid of the data variable, based on its name.
    stat = nf90_inq_varid(this%ncid, var_name, var_id)
    stat = nf90_put_var(this%ncid, var_id, data, start, count, stride, map)
  end subroutine nc_write1D

  subroutine nc_write2D(this, var_name, data, stat, start, count, stride, map)
    ! Write variable data to file
    ! input values: var_name (char*) = name of variable
    !               data (real(:,:)) = the array containing data of the variable
    !               stat (int) = netcdf error code
    implicit none
    class(nc_file) :: this
    character (len = *) :: var_name

    !TODO Figure out some template like way to allow generic type for data
    real, dimension(:,:) :: data
    integer, intent(out) :: stat
    integer :: var_id
    integer, optional, dimension(:) :: start, count, stride, map

    ! Get the varid of the data variable, based on its name.
    stat = nf90_inq_varid(this%ncid, var_name, var_id)
    stat = nf90_put_var(this%ncid, var_id, data, start, count, stride, map)
  end subroutine nc_write2D

  integer function nc_close(this)
  ! Close the file, freeing all resources
    implicit none
    class(nc_file) :: this

    nc_close = nf90_close(this%ncid)
  end function nc_close

end module netcdf_handle
