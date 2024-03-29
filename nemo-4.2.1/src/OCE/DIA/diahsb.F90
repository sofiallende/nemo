MODULE diahsb
   !!======================================================================
   !!                       ***  MODULE  diahsb  ***
   !! Ocean diagnostics: Heat, salt and volume budgets
   !!======================================================================
   !! History :  3.3  ! 2010-09  (M. Leclair)  Original code
   !!                 ! 2012-10  (C. Rousset)  add iom_put
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   dia_hsb       : Diagnose the conservation of ocean heat and salt contents, and volume
   !!   dia_hsb_rst   : Read or write DIA file in restart file
   !!   dia_hsb_init  : Initialization of the conservation diagnostic
   !!----------------------------------------------------------------------
   USE oce            ! ocean dynamics and tracers
   USE dom_oce        ! ocean space and time domain
   USE phycst         ! physical constants
   USE sbc_oce        ! surface thermohaline fluxes
   USE isf_oce        ! ice shelf fluxes
   USE sbcrnf         ! river runoff
   USE domvvl         ! vertical scale factors
   USE traqsr         ! penetrative solar radiation
   USE trabbc         ! bottom boundary condition
   USE trabbc         ! bottom boundary condition
   USE restart        ! ocean restart
   USE bdy_oce , ONLY : ln_bdy
   !
   USE iom            ! I/O manager
   USE in_out_manager ! I/O manager
   USE lib_fortran    ! glob_sum
   USE lib_mpp        ! distributed memory computing library
   USE timing         ! preformance summary

   IMPLICIT NONE
   PRIVATE

   PUBLIC   dia_hsb        ! routine called by step.F90
   PUBLIC   dia_hsb_init   ! routine called by nemogcm.F90

   LOGICAL, PUBLIC ::   ln_diahsb   !: check the heat and salt budgets

   REAL(wp) ::   surf_tot              ! ocean surface
   REAL(wp)  :: frc_s! global forcing trends
   REAL(dp)  :: frc_t, frc_v! global forcing trends
   REAL(wp) ::   frc_wn_t, frc_wn_s    ! global forcing trends
   !
   REAL(dp), DIMENSION(:,:)  , ALLOCATABLE ::   surf
   REAL(wp), DIMENSION(:,:)  , ALLOCATABLE  :: ssh_ini!
   REAL(dp), DIMENSION(:,:)  , ALLOCATABLE  :: surf_ini!
   REAL(wp), DIMENSION(:,:)  , ALLOCATABLE ::   ssh_hc_loc_ini, ssh_sc_loc_ini   !
   REAL(wp), DIMENSION(:,:,:), ALLOCATABLE  :: hc_loc_ini!
   REAL(dp), DIMENSION(:,:,:), ALLOCATABLE  :: sc_loc_ini, e3t_ini!
   REAL(wp), DIMENSION(:,:,:), ALLOCATABLE ::   tmask_ini

   !! * Substitutions
#  include "domzgr_substitute.h90"
#  include "single_precision_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: diahsb.F90 15062 2021-06-28 11:19:48Z jchanut $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE dia_hsb( kt, Kbb, Kmm )
      !!---------------------------------------------------------------------------
      !!                  ***  ROUTINE dia_hsb  ***
      !!
      !! ** Purpose: Compute the ocean global heat content, salt content and volume conservation
      !!
      !! ** Method : - Compute the deviation of heat content, salt content and volume
      !!	            at the current time step from their values at nit000
      !!	            - Compute the contribution of forcing and remove it from these deviations
      !!
      !!---------------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt         ! ocean time-step index
      INTEGER, INTENT(in) ::   Kbb, Kmm   ! ocean time level indices
      !
      INTEGER    ::   ji, jj, jk                  ! dummy loop indice
      REAL(wp)   ::   zdiff_hc    , zdiff_sc      ! heat and salt content variations
      REAL(wp)   ::   zdiff_hc1   , zdiff_sc1     !  -         -     -        -
      REAL(dp)   ::   zdiff_v1    , zdiff_v2      ! volume variation
      REAL(wp)   ::   zerr_hc1    , zerr_sc1      ! heat and salt content misfit
      REAL(wp)   ::   zvol_tot                    ! volume
      REAL(wp)   ::   z_frc_trd_t , z_frc_trd_s   !    -     -
      REAL(dp)   ::   z_frc_trd_v                 !    -     -
      REAL(wp)   ::   z_wn_trd_t , z_wn_trd_s     !    -     -
      REAL(wp)   ::   z_ssh_hc , z_ssh_sc         !    -     -
      REAL(dp), DIMENSION(jpi,jpj,13)      ::   ztmp
      REAL(dp), DIMENSION(jpi,jpj,jpkm1,4) ::   ztmpk
      REAL(dp), DIMENSION(17)              ::   zbg
      !!---------------------------------------------------------------------------
      IF( ln_timing )   CALL timing_start('dia_hsb')
      !
      ztmp (:,:,:)   = 0._wp ! should be better coded
      ztmpk(:,:,:,:) = 0._wp ! should be better coded
      !
      ts(:,:,:,1,Kmm) = ts(:,:,:,1,Kmm) * tmask(:,:,:) ; ts(:,:,:,1,Kbb) = ts(:,:,:,1,Kbb) * tmask(:,:,:) ;
      ts(:,:,:,2,Kmm) = ts(:,:,:,2,Kmm) * tmask(:,:,:) ; ts(:,:,:,2,Kbb) = ts(:,:,:,2,Kbb) * tmask(:,:,:) ;
      !
      ! ------------------------- !
      ! 1 - Trends due to forcing !
      ! ------------------------- !
      ! prepare trends
      ztmp(:,:,1)  = - r1_rho0 * ( emp(:,:) - rnf(:,:) - fwfisf_cav(:,:) - fwfisf_par(:,:) ) * surf(:,:)    ! volume
      ztmp(:,:,2)  =   sbc_tsc(:,:,jp_tem) * surf(:,:)                                                      ! heat
      ztmp(:,:,3)  =   sbc_tsc(:,:,jp_sal) * surf(:,:)                                                      ! salt
      IF( ln_rnf     )    ztmp(:,:,4) =   rnf_tsc(:,:,jp_tem) * surf(:,:)                                   ! runoff temp
      IF( ln_rnf_sal )    ztmp(:,:,5) =   rnf_tsc(:,:,jp_sal) * surf(:,:)                                   ! runoff salt
      IF( ln_isf     )    ztmp(:,:,6) = ( risf_cav_tsc(:,:,jp_tem) + risf_par_tsc(:,:,jp_tem) ) * surf(:,:) ! isf temp
      IF( ln_traqsr  )    ztmp(:,:,7) =   r1_rho0_rcp * qsr(:,:) * surf(:,:)                                ! penetrative solar radiation
      IF( ln_trabbc  )    ztmp(:,:,8) =   qgh_trd0(:,:) * surf(:,:)                                         ! geothermal heat
      !
      IF( ln_linssh ) THEN   ! Advection flux through fixed surface (z=0)
         IF( ln_isfcav ) THEN
            DO ji=1,jpi
               DO jj=1,jpj
                  ztmp(ji,jj,9 ) = - surf(ji,jj) * ww(ji,jj,mikt(ji,jj)) * ts(ji,jj,mikt(ji,jj),jp_tem,Kbb)
                  ztmp(ji,jj,10) = - surf(ji,jj) * ww(ji,jj,mikt(ji,jj)) * ts(ji,jj,mikt(ji,jj),jp_sal,Kbb)
               END DO
            END DO
         ELSE
            ztmp(:,:,9 ) = - surf(:,:) * ww(:,:,1) * ts(:,:,1,jp_tem,Kbb)
            ztmp(:,:,10) = - surf(:,:) * ww(:,:,1) * ts(:,:,1,jp_sal,Kbb)
         END IF
      ENDIF
      
      ! global sum
      zbg(1:10) = glob_sum_vec( 'dia_hsb', ztmp(:,:,1:10) )

      ! adding up
      z_frc_trd_v = zbg(1)  ! volume fluxes
      z_frc_trd_t = zbg(2)  ! heat fluxes
      z_frc_trd_s = zbg(3)  ! salt fluxes
      IF( ln_rnf    )   z_frc_trd_t = z_frc_trd_t + zbg(4) ! runoff heat
      IF( ln_rnf_sal)   z_frc_trd_s = z_frc_trd_s + zbg(5) ! runoff salt
      IF( ln_isf    )   z_frc_trd_t = z_frc_trd_t + zbg(6) ! isf heat
      IF( ln_traqsr )   z_frc_trd_t = z_frc_trd_t + zbg(7) ! penetrative solar flux
      IF( ln_trabbc )   z_frc_trd_t = z_frc_trd_t + zbg(8) ! geothermal heat
      !
      frc_v = frc_v + z_frc_trd_v * rn_Dt
      frc_t = frc_t + z_frc_trd_t * rn_Dt
      frc_s = frc_s + z_frc_trd_s * rn_Dt
      !                                          ! Advection flux through fixed surface (z=0)
      IF( ln_linssh ) THEN
         z_wn_trd_t = zbg(9)
         z_wn_trd_s = zbg(10)
         !
         frc_wn_t = frc_wn_t + z_wn_trd_t * rn_Dt
         frc_wn_s = frc_wn_s + z_wn_trd_s * rn_Dt
      ENDIF

      ! --------------------------------- !
      ! 2 -  Content variations with ssh  !
      ! --------------------------------- !
      ! glob_sum is needed because you keep only the interior domain to compute the sum (iscpl)
      !
      !                    ! volume variation (calculated with ssh)
      ztmp(:,:,11) = surf(:,:)*ssh(:,:,Kmm) - surf_ini(:,:)*ssh_ini(:,:)

      !                    ! heat & salt content variation (associated with ssh)
      IF( ln_linssh ) THEN       ! linear free surface case
         IF( ln_isfcav ) THEN          ! ISF case
            DO ji = 1, jpi
               DO jj = 1, jpj
                  ztmp(ji,jj,12) = surf(ji,jj) * ( ts(ji,jj,mikt(ji,jj),jp_tem,Kmm) * ssh(ji,jj,Kmm) - ssh_hc_loc_ini(ji,jj) )
                  ztmp(ji,jj,13) = surf(ji,jj) * ( ts(ji,jj,mikt(ji,jj),jp_sal,Kmm) * ssh(ji,jj,Kmm) - ssh_sc_loc_ini(ji,jj) )
               END DO
            END DO
         ELSE                          ! no under ice-shelf seas
            ztmp(:,:,12) = surf(:,:) * ( ts(:,:,1,jp_tem,Kmm) * ssh(:,:,Kmm) - ssh_hc_loc_ini(:,:) )
            ztmp(:,:,13) = surf(:,:) * ( ts(:,:,1,jp_sal,Kmm) * ssh(:,:,Kmm) - ssh_sc_loc_ini(:,:) )
         END IF
      ENDIF

      ! global sum
      zbg(11:13) = glob_sum_vec( 'dia_hsb', ztmp(:,:,11:13) )
      
      zdiff_v1 = zbg(11)
      !                    ! heat & salt content variation (associated with ssh)
      IF( ln_linssh ) THEN       ! linear free surface case
         z_ssh_hc = zbg(12)
         z_ssh_sc = zbg(13)
      ENDIF
      !
      ! --------------------------------- !
      ! 3 -  Content variations with e3t  !
      ! --------------------------------- !
      ! glob_sum is needed because you keep only the interior domain to compute the sum (iscpl)
      !
      DO jk = 1, jpkm1           ! volume
         ztmpk(:,:,jk,1) =   surf    (:,:) * e3t(:,:,jk,Kmm)*tmask(:,:,jk)   &
            &              - surf_ini(:,:) * e3t_ini(:,:,jk    )*tmask_ini(:,:,jk)
      END DO
      DO jk = 1, jpkm1           ! heat
         ztmpk(:,:,jk,2) = ( surf    (:,:) * e3t(:,:,jk,Kmm)*ts(:,:,jk,jp_tem,Kmm)   &
            &              - surf_ini(:,:) *         hc_loc_ini(:,:,jk) )
      END DO
      DO jk = 1, jpkm1           ! salt
         ztmpk(:,:,jk,3) = ( surf    (:,:) * e3t(:,:,jk,Kmm)*ts(:,:,jk,jp_sal,Kmm)   &
            &              - surf_ini(:,:) *         sc_loc_ini(:,:,jk) )
      END DO
      DO jk = 1, jpkm1           ! total ocean volume
         ztmpk(:,:,jk,4) = surf(:,:) * e3t(:,:,jk,Kmm) * tmask(:,:,jk)
      END DO
      
      ! global sum
      zbg(14:17) = glob_sum_vec( 'dia_hsb', ztmpk(:,:,:,1:4) )
      
      zdiff_v2 = zbg(14)     ! glob_sum needed as tmask and tmask_ini could be different
      zdiff_hc = zbg(15)
      zdiff_sc = zbg(16)
      zvol_tot = zbg(17)

      ! ------------------------ !
      ! 4 -  Drifts              !
      ! ------------------------ !
      zdiff_v1 = zdiff_v1 - frc_v
      IF( .NOT.ln_linssh )   zdiff_v2 = zdiff_v2 - frc_v
      zdiff_hc = zdiff_hc - frc_t
      zdiff_sc = zdiff_sc - frc_s
      IF( ln_linssh ) THEN
         zdiff_hc1 = zdiff_hc + z_ssh_hc
         zdiff_sc1 = zdiff_sc + z_ssh_sc
         zerr_hc1  = z_ssh_hc - frc_wn_t
         zerr_sc1  = z_ssh_sc - frc_wn_s
      ENDIF

!!gm to be added ?
!      IF( ln_linssh ) THEN            ! fixed volume, add the ssh contribution
!        zvol_tot = zvol_tot + glob_sum( 'diahsb', surf(:,:) * ssh(:,:,Kmm) )
!      ENDIF
!!gm end

      CALL iom_put(   'bgfrcvol' , frc_v    * 1.e-9    )              ! vol - surface forcing (km3)
      CALL iom_put(   'bgfrctem' , frc_t    * rho0 * rcp * 1.e-20 )   ! hc  - surface forcing (1.e20 J)
      CALL iom_put(   'bgfrchfx' , frc_t    * rho0 * rcp /  &         ! hc  - surface forcing (W/m2)
         &                       ( surf_tot * kt * rn_Dt )        )
      CALL iom_put(   'bgfrcsal' , frc_s    * 1.e-9    )              ! sc  - surface forcing (psu*km3)

      IF( .NOT. ln_linssh ) THEN
         CALL iom_put( 'bgtemper' , zdiff_hc / zvol_tot )              ! Temperature drift     (C)
         CALL iom_put( 'bgsaline' , zdiff_sc / zvol_tot )              ! Salinity    drift     (PSU)
         CALL iom_put( 'bgheatco' , zdiff_hc * 1.e-20 * rho0 * rcp )   ! Heat content drift    (1.e20 J)
         CALL iom_put( 'bgheatfx' , zdiff_hc * rho0 * rcp /  &         ! Heat flux drift       (W/m2)
            &                       ( surf_tot * kt * rn_Dt )        )
         CALL iom_put( 'bgsaltco' , zdiff_sc * 1.e-9    )              ! Salt content drift    (psu*km3)
         CALL iom_put( 'bgvolssh' , zdiff_v1 * 1.e-9    )              ! volume ssh drift      (km3)
         CALL iom_put( 'bgvole3t' , zdiff_v2 * 1.e-9    )              ! volume e3t drift      (km3)
         !
         IF( kt == nitend .AND. lwp ) THEN
            WRITE(numout,*)
            WRITE(numout,*) 'dia_hsb : last time step hsb diagnostics: at it= ', kt,' date= ', ndastp
            WRITE(numout,*) '~~~~~~~'
            WRITE(numout,*) '   Temperature drift = ', zdiff_hc / zvol_tot, ' C'
            WRITE(numout,*) '   Salinity    drift = ', zdiff_sc / zvol_tot, ' PSU'
            WRITE(numout,*) '   volume ssh  drift = ', zdiff_v1 * 1.e-9   , ' km^3'
            WRITE(numout,*) '   volume e3t  drift = ', zdiff_v2 * 1.e-9   , ' km^3'
         ENDIF
         !
      ELSE
         CALL iom_put( 'bgtemper' , zdiff_hc1 / zvol_tot)              ! Heat content drift    (C)
         CALL iom_put( 'bgsaline' , zdiff_sc1 / zvol_tot)              ! Salt content drift    (PSU)
         CALL iom_put( 'bgheatco' , zdiff_hc1 * 1.e-20 * rho0 * rcp )  ! Heat content drift    (1.e20 J)
         CALL iom_put( 'bgheatfx' , zdiff_hc1 * rho0 * rcp /  &        ! Heat flux drift       (W/m2)
            &                       ( surf_tot * kt * rn_Dt )         )
         CALL iom_put( 'bgsaltco' , zdiff_sc1 * 1.e-9    )             ! Salt content drift    (psu*km3)
         CALL iom_put( 'bgvolssh' , zdiff_v1 * 1.e-9    )              ! volume ssh drift      (km3)
         CALL iom_put( 'bgmistem' , zerr_hc1 / zvol_tot )              ! hc  - error due to free surface (C)
         CALL iom_put( 'bgmissal' , zerr_sc1 / zvol_tot )              ! sc  - error due to free surface (psu)
      ENDIF
      !
      IF( lrst_oce )   CALL dia_hsb_rst( kt, Kmm, 'WRITE' )
      !
      IF( ln_timing )   CALL timing_stop('dia_hsb')
      !
   END SUBROUTINE dia_hsb


   SUBROUTINE dia_hsb_rst( kt, Kmm, cdrw )
      !!---------------------------------------------------------------------
      !!                   ***  ROUTINE dia_hsb_rst  ***
      !!
      !! ** Purpose :   Read or write DIA file in restart file
      !!
      !! ** Method  :   use of IOM library
      !!----------------------------------------------------------------------
      INTEGER         , INTENT(in) ::   kt     ! ocean time-step
      INTEGER         , INTENT(in) ::   Kmm    ! ocean time level index
      CHARACTER(len=*), INTENT(in) ::   cdrw   ! "READ"/"WRITE" flag
      !
      INTEGER ::   ji, jj, jk   ! dummy loop indices
      !!----------------------------------------------------------------------
      !
      IF( TRIM(cdrw) == 'READ' ) THEN        ! Read/initialise
         IF( ln_rstart ) THEN                   !* Read the restart file
            !
            IF(lwp) WRITE(numout,*)
            IF(lwp) WRITE(numout,*) '   dia_hsb_rst : read hsb restart at it= ', kt,' date= ', ndastp
            IF(lwp) WRITE(numout,*)
            CALL iom_get( numror, 'frc_v', frc_v )
            CALL iom_get( numror, 'frc_t', frc_t )
            CALL iom_get( numror, 'frc_s', frc_s )
            IF( ln_linssh ) THEN
               CALL iom_get( numror, 'frc_wn_t', frc_wn_t )
               CALL iom_get( numror, 'frc_wn_s', frc_wn_s )
            ENDIF
            CALL iom_get( numror, jpdom_auto, 'surf_ini'  , surf_ini   ) ! ice sheet coupling
            CALL iom_get( numror, jpdom_auto, 'ssh_ini'   , ssh_ini    )
            CALL iom_get( numror, jpdom_auto, 'e3t_ini'   , e3t_ini    )
            CALL iom_get( numror, jpdom_auto, 'tmask_ini' , tmask_ini  )
            CALL iom_get( numror, jpdom_auto, 'hc_loc_ini', hc_loc_ini )
            CALL iom_get( numror, jpdom_auto, 'sc_loc_ini', sc_loc_ini )
            IF( ln_linssh ) THEN
               CALL iom_get( numror, jpdom_auto, 'ssh_hc_loc_ini', ssh_hc_loc_ini )
               CALL iom_get( numror, jpdom_auto, 'ssh_sc_loc_ini', ssh_sc_loc_ini )
            ENDIF
         ELSE
            IF(lwp) WRITE(numout,*)
            IF(lwp) WRITE(numout,*) '   dia_hsb_rst : initialise hsb at initial state '
            IF(lwp) WRITE(numout,*)
            surf_ini(:,:) = e1e2t(:,:) * tmask_i(:,:)         ! initial ocean surface
            ssh_ini(:,:) = ssh(:,:,Kmm)                          ! initial ssh
            DO jk = 1, jpk
              ! if ice sheet/oceqn coupling, need to mask ini variables here (mask could change at the next NEMO instance).
               e3t_ini   (:,:,jk) = e3t(:,:,jk,Kmm)                      * tmask(:,:,jk)  ! initial vertical scale factors
               tmask_ini (:,:,jk) = tmask(:,:,jk)                                       ! initial mask
               hc_loc_ini(:,:,jk) = ts(:,:,jk,jp_tem,Kmm) * e3t(:,:,jk,Kmm) * tmask(:,:,jk)  ! initial heat content
               sc_loc_ini(:,:,jk) = ts(:,:,jk,jp_sal,Kmm) * e3t(:,:,jk,Kmm) * tmask(:,:,jk)  ! initial salt content
            END DO
            frc_v = 0._wp                                           ! volume       trend due to forcing
            frc_t = 0._wp                                           ! heat content   -    -   -    -
            frc_s = 0._wp                                           ! salt content   -    -   -    -
            IF( ln_linssh ) THEN
               IF( ln_isfcav ) THEN
                  DO ji = 1, jpi
                     DO jj = 1, jpj
                        ssh_hc_loc_ini(ji,jj) = ts(ji,jj,mikt(ji,jj),jp_tem,Kmm) * ssh(ji,jj,Kmm)   ! initial heat content in ssh
                        ssh_sc_loc_ini(ji,jj) = ts(ji,jj,mikt(ji,jj),jp_sal,Kmm) * ssh(ji,jj,Kmm)   ! initial salt content in ssh
                     END DO
                   END DO
                ELSE
                  ssh_hc_loc_ini(:,:) = ts(:,:,1,jp_tem,Kmm) * ssh(:,:,Kmm)   ! initial heat content in ssh
                  ssh_sc_loc_ini(:,:) = ts(:,:,1,jp_sal,Kmm) * ssh(:,:,Kmm)   ! initial salt content in ssh
               END IF
               frc_wn_t = 0._wp                                       ! initial heat content misfit due to free surface
               frc_wn_s = 0._wp                                       ! initial salt content misfit due to free surface
            ENDIF
         ENDIF
         !
      ELSEIF( TRIM(cdrw) == 'WRITE' ) THEN   ! Create restart file
         !                                   ! -------------------
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) '   dia_hsb_rst : write restart at it= ', kt,' date= ', ndastp
         IF(lwp) WRITE(numout,*)
         !
         CALL iom_rstput( kt, nitrst, numrow, 'frc_v', frc_v )
         CALL iom_rstput( kt, nitrst, numrow, 'frc_t', frc_t )
         CALL iom_rstput( kt, nitrst, numrow, 'frc_s', frc_s )
         IF( ln_linssh ) THEN
            CALL iom_rstput( kt, nitrst, numrow, 'frc_wn_t', frc_wn_t )
            CALL iom_rstput( kt, nitrst, numrow, 'frc_wn_s', frc_wn_s )
         ENDIF
         CALL iom_rstput( kt, nitrst, numrow, 'surf_ini'  , surf_ini   )      ! ice sheet coupling
         CALL iom_rstput( kt, nitrst, numrow, 'ssh_ini'   , ssh_ini    )
         CALL iom_rstput( kt, nitrst, numrow, 'e3t_ini'   , e3t_ini    )
         CALL iom_rstput( kt, nitrst, numrow, 'tmask_ini' , tmask_ini  )
         CALL iom_rstput( kt, nitrst, numrow, 'hc_loc_ini', hc_loc_ini )
         CALL iom_rstput( kt, nitrst, numrow, 'sc_loc_ini', sc_loc_ini )
         IF( ln_linssh ) THEN
            CALL iom_rstput( kt, nitrst, numrow, 'ssh_hc_loc_ini', ssh_hc_loc_ini )
            CALL iom_rstput( kt, nitrst, numrow, 'ssh_sc_loc_ini', ssh_sc_loc_ini )
         ENDIF
         !
      ENDIF
      !
   END SUBROUTINE dia_hsb_rst


   SUBROUTINE dia_hsb_init( Kmm )
      !!---------------------------------------------------------------------------
      !!                  ***  ROUTINE dia_hsb  ***
      !!
      !! ** Purpose: Initialization for the heat salt volume budgets
      !!
      !! ** Method : Compute initial heat content, salt content and volume
      !!
      !! ** Action : - Compute initial heat content, salt content and volume
      !!             - Initialize forcing trends
      !!             - Compute coefficients for conversion
      !!---------------------------------------------------------------------------
      INTEGER, INTENT(in) :: Kmm ! time level index
      !
      INTEGER ::   ierror, ios   ! local integer
      !!
      NAMELIST/namhsb/ ln_diahsb
      !!----------------------------------------------------------------------
      !
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'dia_hsb_init : heat and salt budgets diagnostics'
         WRITE(numout,*) '~~~~~~~~~~~~ '
      ENDIF
      READ  ( numnam_ref, namhsb, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 )   CALL ctl_nam ( ios , 'namhsb in reference namelist' )
      READ  ( numnam_cfg, namhsb, IOSTAT = ios, ERR = 902 )
902   IF( ios >  0 )   CALL ctl_nam ( ios , 'namhsb in configuration namelist' )
      IF(lwm) WRITE( numond, namhsb )

      IF(lwp) THEN
         WRITE(numout,*) '   Namelist  namhsb :'
         WRITE(numout,*) '      check the heat and salt budgets (T) or not (F)       ln_diahsb = ', ln_diahsb
      ENDIF
      !
      IF( .NOT. ln_diahsb )   RETURN

      ! ------------------- !
      ! 1 - Allocate memory !
      ! ------------------- !
      ALLOCATE( hc_loc_ini(jpi,jpj,jpk), sc_loc_ini(jpi,jpj,jpk), surf_ini(jpi,jpj), &
         &      e3t_ini(jpi,jpj,jpk), surf(jpi,jpj),  ssh_ini(jpi,jpj), tmask_ini(jpi,jpj,jpk),STAT=ierror  )
      IF( ierror > 0 ) THEN
         CALL ctl_stop( 'dia_hsb_init: unable to allocate hc_loc_ini' )   ;   RETURN
      ENDIF

      IF( ln_linssh )   ALLOCATE( ssh_hc_loc_ini(jpi,jpj), ssh_sc_loc_ini(jpi,jpj),STAT=ierror )
      IF( ierror > 0 ) THEN
         CALL ctl_stop( 'dia_hsb: unable to allocate ssh_hc_loc_ini' )   ;   RETURN
      ENDIF

      ! ----------------------------------------------- !
      ! 2 - Time independant variables and file opening !
      ! ----------------------------------------------- !
      surf(:,:) = e1e2t(:,:) * tmask_i(:,:)               ! masked surface grid cell area
      surf_tot  = glob_sum( 'diahsb', surf(:,:) )         ! total ocean surface area

      IF( ln_bdy ) CALL ctl_warn( 'dia_hsb_init: heat/salt budget does not consider open boundary fluxes' )
      !
      ! ---------------------------------- !
      ! 4 - initial conservation variables !
      ! ---------------------------------- !
      CALL dia_hsb_rst( nit000, Kmm, 'READ' )  !* read or initialize all required files
      !
   END SUBROUTINE dia_hsb_init

   !!======================================================================
END MODULE diahsb
