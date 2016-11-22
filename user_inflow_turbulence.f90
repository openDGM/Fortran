!> @file user_inflow_turbulence.f90
!--------------------------------------------------------------------------------!
! This file is part of PALM.

! Description:
! ------------
!> Execution of user-defined inflow_turbulece actions executed at the end of inflow_turbulence subroutine
!------------------------------------------------------------------------------!
 SUBROUTINE user_inflow_turbulence(inflow_dist)

    USE arrays_3d,                                                             &
        ONLY:  e, inflow_damping_factor, mean_inflow_profiles, pt, q, u, v, w

    USE control_parameters,                                                    &
        ONLY:  humidity, passive_scalar, recycling_plane, recycling_yshift

    USE indices,                                                               &
        ONLY:  nbgp, nxl, ny, nyn, nys, nyng, nysg, nzb, nzt

    USE kinds

    USE pegrid

    USE user

    IMPLICIT NONE
    INTEGER(iwp) ::  i        !< loop index
    INTEGER(iwp) ::  j        !< loop index
    INTEGER(iwp) ::  k        !< loop index
    INTEGER(iwp) ::  l        !< loop index

    REAL(wp), DIMENSION(nzb:nzt+1,nysg:nyng,6,nbgp) ::                         &
       inflow_dist        !< turbulence signal of vars, added at inflow boundary

!
!-- Add the disturbance at the inflow
    IF ( nxl == 0 )  THEN
       DO  j = nysg, nyng
          DO  k = nzb, nzt + 1

             u(k,j,-nbgp+1:0) = mean_inflow_profiles_2D(k,j,1) +                 &
                        inflow_dist(k,j,1,1:nbgp) * inflow_damping_factor(k)
             v(k,j,-nbgp:-1)  = mean_inflow_profiles_2D(k,j,2) +                  &
                        inflow_dist(k,j,2,1:nbgp) * inflow_damping_factor(k)
             w(k,j,-nbgp:-1)  =                                                   &
                        inflow_dist(k,j,3,1:nbgp) * inflow_damping_factor(k)
             pt(k,j,-nbgp:-1) = mean_inflow_profiles_2D(k,j,4) +                  &
                        inflow_dist(k,j,4,1:nbgp) * inflow_damping_factor(k)
             e(k,j,-nbgp:-1)  = mean_inflow_profiles_2D(k,j,5) +                  &
                        inflow_dist(k,j,5,1:nbgp) * inflow_damping_factor(k)
             e(k,j,-nbgp:-1)  = MAX( e(k,j,-nbgp:-1), 0.0_wp )

             IF ( humidity  .OR.  passive_scalar )                                &
                q(k,j,-nbgp:-1)  = mean_inflow_profiles_2D(k,j,6) +               &
                        inflow_dist(k,j,6,1:nbgp) * inflow_damping_factor(k)

          ENDDO
       ENDDO
    ENDIF

 END SUBROUTINE user_inflow_turbulence

