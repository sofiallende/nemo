MODULE trdken
   !!======================================================================
   !!                       ***  MODULE  trdken  ***
   !! Ocean diagnostics:  compute and output 3D kinetic energy trends
   !!=====================================================================
   !! History :  3.5  !  2012-02  (G. Madec) original code
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   trd_ken       : compute and output 3D Kinetic energy trends using IOM
   !!   trd_ken_init  : initialisation
   !!----------------------------------------------------------------------
   USE oce            ! ocean dynamics and tracers variables
   USE dom_oce        ! ocean space and time domain variables
   USE phycst         ! physical constants
   USE sbc_oce        ! surface boundary condition: ocean
   USE zdf_oce        ! ocean vertical physics variables
!!gm   USE zdfdrg         ! ocean vertical physics: bottom friction
   USE ldftra         ! ocean active tracers lateral physics
   USE trd_oce        ! trends: ocean variables
   USE trdvor         ! ocean vorticity trends 
   USE trdglo         ! trends:global domain averaged
   USE trdmxl         ! ocean active mixed layer tracers trends
   !
   USE in_out_manager ! I/O manager
   USE iom            ! I/O manager library
   USE lib_mpp        ! MPP library
   USE ldfslp         ! Isopycnal slopes

   IMPLICIT NONE
   PRIVATE

   PUBLIC   trd_ken       ! called by trddyn module
   PUBLIC   trd_ken_init  ! called by trdini module

   INTEGER ::   nkstp       ! current time step 

   REAL(wp), ALLOCATABLE, DIMENSION(:,:,:) ::   bu, bv   ! volume of u- and v-boxes
   REAL(wp), ALLOCATABLE, DIMENSION(:,:,:) ::   r1_bt    ! inverse of t-box volume

   !! * Substitutions
#  include "do_loop_substitute.h90"
#  include "domzgr_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: trdken.F90 15104 2021-07-07 14:36:00Z clem $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   INTEGER FUNCTION trd_ken_alloc()
      !!---------------------------------------------------------------------
      !!                  ***  FUNCTION trd_ken_alloc  ***
      !!---------------------------------------------------------------------
      ALLOCATE( bu(jpi,jpj,jpk) , bv(jpi,jpj,jpk) , r1_bt(jpi,jpj,jpk) , STAT= trd_ken_alloc )
      !
      CALL mpp_sum ( 'trdken', trd_ken_alloc )
      IF( trd_ken_alloc /= 0 )   CALL ctl_stop( 'STOP', 'trd_ken_alloc: failed to allocate arrays' )
   END FUNCTION trd_ken_alloc


   SUBROUTINE trd_ken( putrd, pvtrd, ktrd, kt, Kmm )
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE trd_ken  ***
      !! 
      !! ** Purpose :   output 3D Kinetic Energy trends using IOM
      !!
      !! ** Method  : - apply lbc to the input masked velocity trends 
      !!              - compute the associated KE trend:
      !!          zke = 0.5 * (  mi-1[ uu(Kmm) * putrd * bu ] + mj-1[ vv(Kmm) * pvtrd * bv]  ) / bt
      !!      where bu, bv, bt are the volume of u-, v- and t-boxes. 
      !!              - vertical diffusion case (jpdyn_zdf): 
      !!          diagnose separately the KE trend associated with wind stress
      !!              - bottom friction case (jpdyn_bfr):
      !!          explicit case (ln_drgimp=F): bottom trend put in the 1st level 
      !!                                       of putrd, pvtrd
      !
      !
      !!----------------------------------------------------------------------
      REAL(dp), DIMENSION(:,:,:), INTENT(inout) ::   putrd, pvtrd   ! U and V masked trends
      INTEGER                   , INTENT(in   ) ::   ktrd           ! trend index
      INTEGER                   , INTENT(in   ) ::   kt             ! time step
      INTEGER                   , INTENT(in   ) ::   Kmm            ! time level index
      !
      INTEGER ::   ji, jj, jk       ! dummy loop indices
      INTEGER ::   ikbu  , ikbv     ! local integers
      INTEGER ::   ikbum1, ikbvm1   !   -       -
      REAL(wp), DIMENSION(:,:), ALLOCATABLE ::   z2dx, z2dy, zke2d   ! 2D workspace 
      REAL(wp), DIMENSION(jpi,jpj,jpk)      ::   zke                 ! 3D workspace 
      !!----------------------------------------------------------------------
      !
      CALL lbc_lnk( 'trdken', putrd, 'U', -1.0_dp , pvtrd, 'V', -1.0_dp )      ! lateral boundary conditions
      !
      nkstp = kt
      DO jk = 1, jpkm1
         bu   (:,:,jk) =    e1e2u(:,:) * e3u(:,:,jk,Kmm)
         bv   (:,:,jk) =    e1e2v(:,:) * e3v(:,:,jk,Kmm)
         r1_bt(:,:,jk) = r1_e1e2t(:,:) / e3t(:,:,jk,Kmm) * tmask(:,:,jk)
      END DO
      !
      zke(:,:,jpk) = 0._wp
      zke(1:nn_hls,:, : ) = 0._wp
      zke(:,1:nn_hls, : ) = 0._wp
      DO_3D( 0, nn_hls, 0, nn_hls, 1, jpkm1 )
         zke(ji,jj,jk) = 0.5_wp * rho0 *( uu(ji  ,jj,jk,Kmm) * putrd(ji  ,jj,jk) * bu(ji  ,jj,jk)  &
            &                           + uu(ji-1,jj,jk,Kmm) * putrd(ji-1,jj,jk) * bu(ji-1,jj,jk)  &
            &                           + vv(ji,jj  ,jk,Kmm) * pvtrd(ji,jj  ,jk) * bv(ji,jj  ,jk)  &
            &                           + vv(ji,jj-1,jk,Kmm) * pvtrd(ji,jj-1,jk) * bv(ji,jj-1,jk)  ) * r1_bt(ji,jj,jk)
      END_3D
      !
      SELECT CASE( ktrd )
         CASE( jpdyn_hpg )   ;   CALL iom_put( "ketrd_hpg"   , zke )    ! hydrostatic pressure gradient
         CASE( jpdyn_spg )   ;   CALL iom_put( "ketrd_spg"   , zke )    ! surface pressure gradient
         CASE( jpdyn_pvo )   ;   CALL iom_put( "ketrd_pvo"   , zke )    ! planetary vorticity
         CASE( jpdyn_rvo )   ;   CALL iom_put( "ketrd_rvo"   , zke )    ! relative  vorticity     (or metric term)
         CASE( jpdyn_keg )   ;   CALL iom_put( "ketrd_keg"   , zke )    ! Kinetic Energy gradient (or had)
         CASE( jpdyn_zad )   ;   CALL iom_put( "ketrd_zad"   , zke )    ! vertical   advection
         CASE( jpdyn_ldf )   ;   CALL iom_put( "ketrd_ldf"   , zke )    ! lateral diffusion
         CASE( jpdyn_zdf )   ;   CALL iom_put( "ketrd_zdf"   , zke )    ! vertical diffusion 
         !                   !                                          ! wind stress trends
                                 ALLOCATE( z2dx(jpi,jpj) , z2dy(jpi,jpj) , zke2d(jpi,jpj) )
                           z2dx(:,:) = uu(:,:,1,Kmm) * ( utau_b(:,:) + utau(:,:) ) * e1e2u(:,:) * umask(:,:,1)
                           z2dy(:,:) = vv(:,:,1,Kmm) * ( vtau_b(:,:) + vtau(:,:) ) * e1e2v(:,:) * vmask(:,:,1)
                           zke2d(1:nn_hls,:) = 0._wp   ;   zke2d(:,1:nn_hls) = 0._wp
                           DO_2D( 0, nn_hls, 0, nn_hls )
                              zke2d(ji,jj) = r1_rho0 * 0.5_wp * (   z2dx(ji,jj) + z2dx(ji-1,jj)   &
                              &                                   + z2dy(ji,jj) + z2dy(ji,jj-1)   ) * r1_bt(ji,jj,1)
                           END_2D
                                 CALL iom_put( "ketrd_tau"   , zke2d )  ! 
                                 DEALLOCATE( z2dx , z2dy , zke2d )
         CASE( jpdyn_bfr )   ;   CALL iom_put( "ketrd_bfr"   , zke )    ! bottom friction (explicit case) 
!!gm TO BE DONE properly
!!gm only valid if ln_drgimp=F otherwise the bottom stress as to be recomputed at the end of the computation....
!         IF(.NOT. ln_drgimp) THEN
!            DO jj = 1, jpj    !   
!               DO ji = 1, jpi
!                  ikbu = mbku(ji,jj)         ! deepest ocean u- & v-levels
!                  ikbv = mbkv(ji,jj)   
!                  z2dx(ji,jj) = uu(ji,jj,ikbu,Kmm) * bfrua(ji,jj) * uu(ji,jj,ikbu,Kmm)
!                  z2dy(ji,jj) = vv(ji,jj,ikbu,Kmm) * bfrva(ji,jj) * vv(ji,jj,ikbv,Kmm)
!               END DO
!            END DO
!            zke2d(1,:) = 0._wp   ;   zke2d(:,1) = 0._wp
!            DO jj = 2, jpj
!               DO ji = 2, jpi
!                  zke2d(ji,jj) = 0.5_wp * (   z2dx(ji,jj) + z2dx(ji-1,jj)   &
!                     &                      + z2dy(ji,jj) + z2dy(ji,jj-1)   ) * r1_bt(ji,jj,  BEURK!!!
!               END DO
!            END DO
!                                    CALL iom_put( "ketrd_bfr"  , zke2d )   ! bottom friction (explicit case)
!         ENDIF
!!gm end
         CASE( jpdyn_atf )   ;   CALL iom_put( "ketrd_atf"   , zke )    ! asselin filter trends 
!! a faire !!!!  idee changer dynnxt pour avoir un appel a jpdyn_bfr avant le swap !!!
!! reflechir a une possible sauvegarde du "vrai" uu(Kmm),vv(Kmm) pour le calcul de atf....
!
!         IF( ln_drgimp ) THEN                                          ! bottom friction (implicit case)
!            DO jj = 1, jpj                                                  ! after velocity known (now filed at this stage)
!               DO ji = 1, jpi
!                  ikbu = mbku(ji,jj)          ! deepest ocean u- & v-levels
!                  ikbv = mbkv(ji,jj)
!                  z2dx(ji,jj) = uu(ji,jj,ikbu,Kmm) * bfrua(ji,jj) * uu(ji,jj,ikbu,Kmm) / e3u(ji,jj,ikbu,Kmm)
!                  z2dy(ji,jj) = uu(ji,jj,ikbu,Kmm) * bfrva(ji,jj) * vv(ji,jj,ikbv,Kmm) / e3v(ji,jj,ikbv,Kmm)
!               END DO
!            END DO
!            zke2d(1,:) = 0._wp   ;   zke2d(:,1) = 0._wp
!            DO jj = 2, jpj
!               DO ji = 2, jpi
!                  zke2d(ji,jj) = 0.5_wp * (   z2dx(ji,jj) + z2dx(ji-1,jj)   &
!                     &                      + z2dy(ji,jj) + z2dy(ji,jj-1)   )
!               END DO
!            END DO
!                              CALL iom_put( "ketrd_bfri", zke2d )
!         ENDIF
        CASE( jpdyn_ken )   ;   ! kinetic energy
                    ! called in dynnxt.F90 before asselin time filter
                    ! with putrd=uu(Krhs) and pvtrd=vv(Krhs)
                    zke(:,:,:) = 0.5_wp * zke(:,:,:)
                    CALL iom_put( "KE", zke )
                    !
                    CALL ken_p2k( kt , zke, Kmm )
                      CALL iom_put( "ketrd_convP2K", zke )     ! conversion -rau*g*w
         !
      END SELECT
      !
   END SUBROUTINE trd_ken


   SUBROUTINE ken_p2k( kt , pconv, Kmm )
      !!---------------------------------------------------------------------
      !!                 ***  ROUTINE ken_p2k  ***
      !!                    
      !! ** Purpose :   compute rate of conversion from potential to kinetic energy
      !!
      !! ** Method  : - compute conv defined as -rau*g*w on T-grid points
      !! 
      !! ** Work only for full steps and partial steps (ln_hpg_zco or ln_hpg_zps)
      !!---------------------------------------------------------------------- 
      INTEGER                   , INTENT(in   ) ::   kt      ! ocean time-step index
      INTEGER                   , INTENT(in   ) ::   Kmm     ! time level index
      REAL(wp), DIMENSION(:,:,:), INTENT(  out) ::   pconv   ! 
      !
      INTEGER  ::   ji, jj, jk   ! dummy loop indices
      INTEGER  ::   iku, ikv     ! local integers
      REAL(wp) ::   zcoef        ! local scalars
      REAL(wp), DIMENSION(jpi,jpj,jpk) ::  zconv  ! 3D workspace
      !!----------------------------------------------------------------------
      !
      ! Local constant initialization 
      zcoef = - rho0 * grav * 0.5_wp      
      
      !  Surface value (also valid in partial step case)
      zconv(:,:,1) = zcoef * ( 2._wp * rhd(:,:,1) ) * ww(:,:,1) * e3w(:,:,1,Kmm)

      ! interior value (2=<jk=<jpkm1)
      DO jk = 2, jpk
         zconv(:,:,jk) = zcoef * ( rhd(:,:,jk) + rhd(:,:,jk-1) ) * ww(:,:,jk) * e3w(:,:,jk,Kmm)
      END DO

      ! conv value on T-point
      DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpkm1 )
         zcoef = 0.5_wp / e3t(ji,jj,jk,Kmm)
         pconv(ji,jj,jk) = zcoef * ( zconv(ji,jj,jk) + zconv(ji,jj,jk+1) ) * tmask(ji,jj,jk)
      END_3D
      !
   END SUBROUTINE ken_p2k


   SUBROUTINE trd_ken_init
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE trd_ken_init  ***
      !! 
      !! ** Purpose :   initialisation of 3D Kinetic Energy trend diagnostic
      !!----------------------------------------------------------------------
      INTEGER  ::   ji, jj, jk   ! dummy loop indices
      !!----------------------------------------------------------------------
      !
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) 'trd_ken_init : 3D Kinetic Energy trends'
         WRITE(numout,*) '~~~~~~~~~~~~~'
      ENDIF
      !                           ! allocate box volume arrays
      IF( trd_ken_alloc() /= 0 )   CALL ctl_stop('trd_ken_alloc: failed to allocate arrays')
      !
   END SUBROUTINE trd_ken_init

   !!======================================================================
END MODULE trdken
