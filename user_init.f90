!> @file user_init.f90
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
! $Id: user_init.f90 1818 2016-04-06 15:53:27Z maronga $
!
! 1799 2016-04-05 08:35:55Z gronemeier
! Bugfix: added dots_num to variable list
!
! 1783 2016-03-06 18:36:17Z raasch
! netcdf module name changed + related changes
!
! 1682 2015-10-07 23:56:08Z knoop
! Code annotations made doxygen readable 
! 
! 1353 2014-04-08 15:21:23Z heinze
! REAL constants provided with KIND-attribute 
!
! 1320 2014-03-20 08:40:49Z raasch
! revision history before 2012 removed,
! comment fields (!:) to be used for variable explanations added to
! all variable declaration statements 
!
! 1036 2012-10-22 13:43:42Z raasch
! code put under GPL (PALM 3.9)
!
! 211 2008-11-11 04:46:24Z raasch
! Former file user_interface.f90 split into one file per subroutine
!
! Description:
! ------------
!> Execution of user-defined initializing actions
!------------------------------------------------------------------------------!
 SUBROUTINE user_init
 
    USE arrays_3d

    USE control_parameters
    
    USE indices

    USE kinds
    
    USE netcdf_interface,                                                      &
        ONLY: dots_label, dots_unit, dots_num
    
    USE pegrid
    
    USE user

    USE statistics,                                                            &
        ONLY: hom_sum

#if defined( __netcdf4_parallel )
    USE netcdf
#endif

    IMPLICIT NONE
    INTEGER(iwp)  :: i !loop index
    INTEGER(iwp)  :: j !loop index

    INTEGER :: zuid, yid, varid, numzu, numy
    integer, dimension(nf90_max_var_dims) :: dimIDs
    character(len = 2) :: dimname

#if defined( __netcdf4_parallel )
    CHARACTER(LEN=2), DIMENSION(6) :: nc_var_name = (/"u","v","w","pt","e","q"/)  !inflow profile parameters
#endif

!
!-- Here the user-defined initializing actions follow:
!-- Sample for user-defined output
!    ALLOCATE( u2(nzb:nzt+1,nysg:nyng,nxlg:nxrg) )
!    ALLOCATE( ustvst(nzb:nzt+1,nysg:nyng,nxlg:nxrg) );  ustvst = 0.0_wp

!-- Sample for user-defined time series
!-- For each time series quantity you have to give a label and a unit,
!-- which will be used for the NetCDF file. They must not contain more than
!-- seven characters. The value of dots_num has to be increased by the
!-- number of new time series quantities. Its old value has to be store in
!-- dots_num_palm. See routine user_statistics on how to output calculate
!-- and output these quantities.
!    dots_label(dots_num+1) = 'abs_umx'
!    dots_unit(dots_num+1)  = 'm/s'
!    dots_label(dots_num+2) = 'abs_vmx'
!    dots_unit(dots_num+2)  = 'm/s'
!
!    dots_num_palm = dots_num
!    dots_num = dots_num + 2

! init inflow profiles from netcdf
#if defined( __netcdf4_parallel )
    IF ( use_netcdf_profile_data )  THEN
      CALL nc_handle_err( nf90_open_par("inflow_profile.nc", IOR( NF90_HDF5, NF90_MPIIO ), NCID = nc_id_inflow, COMM = comm2d, INFO = MPI_INFO_NULL) )
      DO i=1,6
          IF ( nf90_inq_varid(nc_id_inflow, nc_var_name(i), nc_var_id) == NF90_NOERR ) THEN
             CALL nc_handle_err( nf90_var_par_access(nc_id_inflow, nc_var_id, NF90_INDEPENDENT) )
             CALL nc_handle_err( nf90_get_var(nc_id_inflow, nc_var_id, mean_inflow_profiles(:,i)) )
          END IF
      END DO
      CALL nc_handle_err( nf90_close(nc_id_inflow) )

    ELSE IF ( use_netcdf_profile_data_2D ) THEN

       IF (myidx == id_inflow) THEN
          ALLOCATE( mean_inflow_profiles_2D( nzb:nzt+1, nysg:nyng, 6) )

          DO  j = nysg, nyng
             IF ( use_prescribed_profile_data )  THEN
                mean_inflow_profiles_2D(:,j,1) = u_init            ! u
                mean_inflow_profiles_2D(:,j,2) = v_init            ! v
             ELSE
                mean_inflow_profiles_2D(:,j,1) = hom_sum(:,1,0)    ! u
                mean_inflow_profiles_2D(:,j,2) = hom_sum(:,2,0)    ! v
             ENDIF
             mean_inflow_profiles_2D(:,j,4) = hom_sum(:,4,0)       ! pt
             mean_inflow_profiles_2D(:,j,5) = hom_sum(:,8,0)       ! e
             mean_inflow_profiles_2D(:,j,6) = hom_sum(:,41,0)      ! q
          END DO

      END IF

      CALL nc_handle_err( nf90_open_par("inflow_profile_2d.nc", IOR( NF90_HDF5, NF90_MPIIO ), NCID = nc_id_inflow, COMM = comm2d, INFO = MPI_INFO_NULL) )
 
      IF (myidx == id_inflow) THEN

      DO i=1,6
          IF ( nf90_inq_varid(nc_id_inflow, nc_var_name(i), nc_var_id) == NF90_NOERR ) THEN
             ! Careful here. Array in NetCDF is row-major.
             CALL nc_handle_err( nf90_get_var(nc_id_inflow, nc_var_id, mean_inflow_profiles_2D(:,:,i), (/nysg+nbgp,nzb+1/), (/nyng-nysg+1,nzt+1-nzb/)))
          END IF
      END DO

      END IF

      CALL nc_handle_err( nf90_close(nc_id_inflow) )

   END IF

#endif

 END SUBROUTINE user_init

