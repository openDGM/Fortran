!> @file user_module_mod.f90
!--------------------------------------------------------------------------------!
! This file is part of PALM.
!
! PALM is free software: you can redistribute it and/or modify it under the terms
! of the GNU General Public License as published by the Free Software Foundation,
! either version 3 of the License, or (at your option) any later version.
!
! PALM is distributed in the hope that it will be useful, but WITHOUT ANY
! WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
! A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License along with
! PALM. If not, see <http://www.gnu.org/licenses/>.
!
! Copyright 1997-2016 Leibniz Universitaet Hannover
!--------------------------------------------------------------------------------!
!
! Current revisions:
! -----------------
! 
! 
! Former revisions:
! -----------------
! $Id: user_module_mod.f90 1851 2016-04-08 13:32:50Z maronga $
!
! 1850 2016-04-08 13:29:27Z maronga
! Module renamed
! 
! 
! 1682 2015-10-07 23:56:08Z knoop
! Code annotations made doxygen readable 
!
! 1320 2014-03-20 08:40:49Z raasch
! kind-parameters added to all INTEGER and REAL declaration statements, 
! kinds are defined in new module kinds, 
! old module precision_kind is removed,
! revision history before 2012 removed,
! comment fields (!:) to be used for variable explanations added to
! all variable declaration statements 
!
! 1036 2012-10-22 13:43:42Z raasch
! code put under GPL (PALM 3.9)
!
! Revision 1.1  1998/03/24 15:29:04  raasch
! Initial revision
!
!
! Description:
! ------------
!> Declaration of user-defined variables. This module may only be used
!> in the user-defined routines (contained in user_interface.f90).
!------------------------------------------------------------------------------!
 MODULE user
 

    USE kinds

#if defined( __netcdf4_parallel )
    USE netcdf
#endif

    IMPLICIT NONE

#if defined( __netcdf4_parallel )
    INTEGER ::  nc_id_inflow      !netcdf_id of inflow profile data
    INTEGER ::  nc_var_id         !netcdf variable id
    LOGICAL ::  use_netcdf_profile_data = .FALSE.
    LOGICAL ::  use_netcdf_profile_data_2D = .FALSE.
#endif

    LOGICAL ::  user_defined_namelist_found = .FALSE.   !< 

!
!-- Sample for user-defined output
!    REAL(wp), DIMENSION(:,:,:), ALLOCATABLE ::  u2       !< user defined array
!    REAL(wp), DIMENSION(:,:,:), ALLOCATABLE ::  u2_av    !< user defined array
!    REAL(wp), DIMENSION(:,:,:), ALLOCATABLE ::  ustvst   !< user defined array

    REAL(wp), DIMENSION(:,:,:), ALLOCATABLE :: mean_inflow_profiles_2D    !<Array for inflow profile of turbulence recycling with varying crossflow dimension

    SAVE

 CONTAINS

#if defined( __netcdf4_parallel )
  SUBROUTINE nc_handle_err(errcode)
  ! Output netcdf error message
  IMPLICIT NONE
    INTEGER, INTENT(in) :: errcode
    
    IF(errcode /= NF90_NOERR) THEN
       PRINT *, 'Error: ', TRIM(NF90_STRERROR(errcode))
       STOP
    ENDIF
  END SUBROUTINE nc_handle_err
#endif

 END MODULE user
