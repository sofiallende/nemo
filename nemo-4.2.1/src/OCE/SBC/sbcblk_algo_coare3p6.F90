MODULE sbcblk_algo_coare3p6
   !!======================================================================
   !!                   ***  MODULE  sbcblk_algo_coare3p6  ***
   !!
   !!                After Fairall et al 2018 & Edson et al 2013
   !! Computes:
   !!   * bulk transfer coefficients C_D, C_E and C_H
   !!   * air temp. and spec. hum. adjusted from zt (2m) to zu (10m) if needed
   !!   * the effective bulk wind speed at 10m Ubzu
   !!   => all these are used in bulk formulas in sbcblk.F90
   !!
   !!       Routine turb_coare3p6 maintained and developed in AeroBulk
   !!                     (https://github.com/brodeau/aerobulk)
   !!
   !! ** Author: L. Brodeau, June 2019 / AeroBulk (https://github.com/brodeau/aerobulk)
   !!----------------------------------------------------------------------
   !! History :  4.0  !  2016-02  (L.Brodeau)   Original code
   !!            4.2  !  2020-12  (L. Brodeau) Introduction of various air-ice bulk parameterizations + improvements
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   turb_coare3p6  : computes the bulk turbulent transfer coefficients
   !!                   adjusts t_air and q_air from zt to zu m
   !!                   returns the effective bulk wind speed at 10m
   !!----------------------------------------------------------------------
   USE dom_oce         ! ocean space and time domain
   USE phycst          ! physical constants
   USE lib_mpp,        ONLY: ctl_stop         ! distribued memory computing library
   USE in_out_manager, ONLY: nit000  ! I/O manager
   USE sbc_phy         ! Catalog of functions for physical/meteorological parameters in the marine boundary layer
   USE sbcblk_skin_coare ! cool-skin/warm layer scheme (CSWL_ECMWF) !LB

   IMPLICIT NONE
   PRIVATE

   PUBLIC :: SBCBLK_ALGO_COARE3P6_INIT, TURB_COARE3P6
   !! * Substitutions
#  include "do_loop_substitute.h90"

   !! COARE own values for given constants:
   REAL(wp), PARAMETER :: zi0   = 600._wp     ! scale height of the atmospheric boundary layer...
   REAL(wp), PARAMETER :: Beta0 =  1.2_wp     ! gustiness parameter
   REAL(wp), PARAMETER :: zeta_abs_max = 50._wp

   !!----------------------------------------------------------------------
CONTAINS


   SUBROUTINE sbcblk_algo_coare3p6_init(l_use_cs, l_use_wl)
      !!---------------------------------------------------------------------
      !!                  ***  FUNCTION sbcblk_algo_coare3p6_init  ***
      !!
      !! INPUT :
      !! -------
      !!    * l_use_cs : use the cool-skin parameterization
      !!    * l_use_wl : use the warm-layer parameterization
      !!---------------------------------------------------------------------
      LOGICAL , INTENT(in) ::   l_use_cs ! use the cool-skin parameterization
      LOGICAL , INTENT(in) ::   l_use_wl ! use the warm-layer parameterization
      INTEGER :: ierr
      !!---------------------------------------------------------------------
      IF( l_use_wl ) THEN
         ierr = 0
         ALLOCATE ( Tau_ac(jpi,jpj) , Qnt_ac(jpi,jpj), dT_wl(jpi,jpj), Hz_wl(jpi,jpj), STAT=ierr )
         IF( ierr > 0 ) CALL ctl_stop( ' SBCBLK_ALGO_COARE3P6_INIT => allocation of Tau_ac, Qnt_ac, dT_wl & Hz_wl failed!' )
         Tau_ac(:,:) = 0._wp
         Qnt_ac(:,:) = 0._wp
         dT_wl(:,:)  = 0._wp
         Hz_wl(:,:)  = Hwl_max
      ENDIF
      IF( l_use_cs ) THEN
         ierr = 0
         ALLOCATE ( dT_cs(jpi,jpj), STAT=ierr )
         IF( ierr > 0 ) CALL ctl_stop( ' SBCBLK_ALGO_COARE3P6_INIT => allocation of dT_cs failed!' )
         dT_cs(:,:) = -0.25_wp  ! First guess of skin correction
      ENDIF
   END SUBROUTINE sbcblk_algo_coare3p6_init



   SUBROUTINE turb_coare3p6( kt, zt, zu, T_s, t_zt, q_s, q_zt, U_zu, l_use_cs, l_use_wl, &
      &                      Cd, Ch, Ce, t_zu, q_zu, Ubzu,                               &
      &                      nb_iter, Cdn, Chn, Cen,                                     & ! optional output
      &                      Qsw, rad_lw, slp, pdT_cs,                                   & ! optionals for cool-skin (and warm-layer)
      &                      pdT_wl, pHz_wl )                                              ! optionals for warm-layer only
      !!----------------------------------------------------------------------
      !!                      ***  ROUTINE  turb_coare3p6  ***
      !!
      !! ** Purpose :   Computes turbulent transfert coefficients of surface
      !!                fluxes according to Fairall et al. (2003)
      !!                If relevant (zt /= zu), adjust temperature and humidity from height zt to zu
      !!                Returns the effective bulk wind speed at zu to be used in the bulk formulas
      !!
      !!                Applies the cool-skin warm-layer correction of the SST to T_s
      !!                if the net shortwave flux at the surface (Qsw), the downwelling longwave
      !!                radiative fluxes at the surface (rad_lw), and the sea-leve pressure (slp)
      !!                are provided as (optional) arguments!
      !!
      !! INPUT :
      !! -------
      !!    *  kt   : current time step (starts at 1)
      !!    *  zt   : height for temperature and spec. hum. of air            [m]
      !!    *  zu   : height for wind speed (usually 10m)                     [m]
      !!    *  t_zt : potential air temperature at zt                         [K]
      !!    *  q_zt : specific humidity of air at zt                          [kg/kg]
      !!    *  U_zu : scalar wind speed at zu                                 [m/s]
      !!    * l_use_cs : use the cool-skin parameterization
      !!    * l_use_wl : use the warm-layer parameterization
      !!
      !! INPUT/OUTPUT:
      !! -------------
      !!    *  T_s  : always "bulk SST" as input                              [K]
      !!              -> unchanged "bulk SST" as output if CSWL not used      [K]
      !!              -> skin temperature as output if CSWL used              [K]
      !!
      !!    *  q_s  : SSQ aka saturation specific humidity at temp. T_s       [kg/kg]
      !!              -> doesn't need to be given a value if skin temp computed (in case l_use_cs=True or l_use_wl=True)
      !!              -> MUST be given the correct value if not computing skint temp. (in case l_use_cs=False or l_use_wl=False)
      !!
      !! OPTIONAL INPUT:
      !! ---------------
      !!    *  Qsw    : net solar flux (after albedo) at the surface (>0)     [W/m^2]
      !!    *  rad_lw : downwelling longwave radiation at the surface  (>0)   [W/m^2]
      !!    *  slp    : sea-level pressure                                    [Pa]
      !!
      !! OPTIONAL OUTPUT:
      !! ----------------
      !!    * pdT_cs  : SST increment "dT" for cool-skin correction           [K]
      !!    * pdT_wl  : SST increment "dT" for warm-layer correction          [K]
      !!    * pHz_wl  : thickness of warm-layer                               [m]
      !!
      !! OUTPUT :
      !! --------
      !!    *  Cd     : drag coefficient
      !!    *  Ch     : sensible heat coefficient
      !!    *  Ce     : evaporation coefficient
      !!    *  t_zu   : pot. air temperature adjusted at wind height zu       [K]
      !!    *  q_zu   : specific humidity of air        //                    [kg/kg]
      !!    *  Ubzu  : bulk wind speed at zu                                 [m/s]
      !!
      !!
      !! ** Author: L. Brodeau, June 2019 / AeroBulk (https://github.com/brodeau/aerobulk/)
      !!----------------------------------------------------------------------------------
      INTEGER,  INTENT(in   )                     ::   kt       ! current time step
      REAL(wp), INTENT(in   )                     ::   zt       ! height for t_zt and q_zt                    [m]
      REAL(wp), INTENT(in   )                     ::   zu       ! height for U_zu                             [m]
      REAL(wp), INTENT(inout), DIMENSION(jpi,jpj) ::   T_s      ! sea surface temperature                [Kelvin]
      REAL(wp), INTENT(in   ), DIMENSION(jpi,jpj) ::   t_zt     ! potential air temperature              [Kelvin]
      REAL(wp), INTENT(inout), DIMENSION(jpi,jpj) ::   q_s      ! sea surface specific humidity           [kg/kg]
      REAL(wp), INTENT(in   ), DIMENSION(jpi,jpj) ::   q_zt     ! specific air humidity at zt             [kg/kg]
      REAL(wp), INTENT(in   ), DIMENSION(jpi,jpj) ::   U_zu     ! relative wind module at zu                [m/s]
      LOGICAL , INTENT(in   )                     ::   l_use_cs ! use the cool-skin parameterization
      LOGICAL , INTENT(in   )                     ::   l_use_wl ! use the warm-layer parameterization
      REAL(wp), INTENT(  out), DIMENSION(jpi,jpj) ::   Cd       ! transfer coefficient for momentum         (tau)
      REAL(wp), INTENT(  out), DIMENSION(jpi,jpj) ::   Ch       ! transfer coefficient for sensible heat (Q_sens)
      REAL(wp), INTENT(  out), DIMENSION(jpi,jpj) ::   Ce       ! transfert coefficient for evaporation   (Q_lat)
      REAL(wp), INTENT(  out), DIMENSION(jpi,jpj) ::   t_zu     ! pot. air temp. adjusted at zu               [K]
      REAL(wp), INTENT(  out), DIMENSION(jpi,jpj) ::   q_zu     ! spec. humidity adjusted at zu           [kg/kg]
      REAL(wp), INTENT(  out), DIMENSION(jpi,jpj) ::   Ubzu    ! bulk wind speed at zu                     [m/s]
      !
      INTEGER , INTENT(in   ), OPTIONAL                     :: nb_iter  ! number of iterations
      REAL(wp), INTENT(  out), OPTIONAL, DIMENSION(jpi,jpj) ::   Cdn
      REAL(wp), INTENT(  out), OPTIONAL, DIMENSION(jpi,jpj) ::   Chn
      REAL(wp), INTENT(  out), OPTIONAL, DIMENSION(jpi,jpj) ::   Cen
      REAL(wp), INTENT(in   ), OPTIONAL, DIMENSION(jpi,jpj) ::   Qsw      !             [W/m^2]
      REAL(wp), INTENT(in   ), OPTIONAL, DIMENSION(jpi,jpj) ::   rad_lw   !             [W/m^2]
      REAL(wp), INTENT(in   ), OPTIONAL, DIMENSION(jpi,jpj) ::   slp      !             [Pa]
      REAL(wp), INTENT(  out), OPTIONAL, DIMENSION(jpi,jpj) ::   pdT_cs
      REAL(wp), INTENT(  out), OPTIONAL, DIMENSION(jpi,jpj) ::   pdT_wl   !             [K]
      REAL(wp), INTENT(  out), OPTIONAL, DIMENSION(jpi,jpj) ::   pHz_wl   !             [m]
      !
      INTEGER :: nbit, jit
      LOGICAL :: l_zt_equal_zu = .FALSE.      ! if q and t are given at same height as U
      !
      REAL(wp), DIMENSION(jpi,jpj) :: u_star, t_star, q_star
      REAL(wp), DIMENSION(jpi,jpj) :: dt_zu, dq_zu
      REAL(wp), DIMENSION(jpi,jpj) :: znu_a         !: Nu_air, Viscosity of air
      REAL(wp), DIMENSION(jpi,jpj) :: z0, z0t
      REAL(wp), DIMENSION(jpi,jpj) :: zeta_u        ! stability parameter at height zu
      REAL(wp), DIMENSION(jpi,jpj) :: ztmp0, ztmp1, ztmp2
      REAL(wp), DIMENSION(jpi,jpj) :: zpre, zrhoa, zta ! air pressure [Pa], density [kg/m3] & absolute temperature [k]
      !
      REAL(wp), DIMENSION(:,:), ALLOCATABLE :: zeta_t  ! stability parameter at height zt
      REAL(wp), DIMENSION(:,:), ALLOCATABLE :: zsst    ! to back up the initial bulk SST
      !
      CHARACTER(len=40), PARAMETER :: crtnm = 'turb_coare3p6@sbcblk_algo_coare3p6'
      !!----------------------------------------------------------------------------------
      IF( kt == nit000 ) CALL SBCBLK_ALGO_COARE3P6_INIT(l_use_cs, l_use_wl)

      nbit = nb_iter0
      IF( PRESENT(nb_iter) ) nbit = nb_iter

      l_zt_equal_zu = ( ABS(zu - zt) < 0.01_wp ) ! testing "zu == zt" is risky with double precision
      IF( .NOT. l_zt_equal_zu )  ALLOCATE( zeta_t(jpi,jpj) )

      !! Initializations for cool skin and warm layer:
      IF( l_use_cs .AND. (.NOT.(PRESENT(Qsw) .AND. PRESENT(rad_lw) .AND. PRESENT(slp))) ) &
         &   CALL ctl_stop( '['//TRIM(crtnm)//'] => ' , 'you need to provide Qsw, rad_lw & slp to use cool-skin param!' )

      IF( l_use_wl .AND. (.NOT.(PRESENT(Qsw) .AND. PRESENT(rad_lw) .AND. PRESENT(slp))) ) &
         &   CALL ctl_stop( '['//TRIM(crtnm)//'] => ' , 'you need to provide Qsw, rad_lw & slp to use warm-layer param!' )

      IF( l_use_cs .OR. l_use_wl ) THEN
         ALLOCATE ( zsst(jpi,jpj) )
         zsst = T_s ! backing up the bulk SST
         IF( l_use_cs ) T_s = T_s - 0.25_wp   ! First guess of correction
         q_s    = rdct_qsat_salt*q_sat(MAX(T_s, 200._wp), slp) ! First guess of q_s
      ENDIF

      !! First guess of temperature and humidity at height zu:
      t_zu = MAX( t_zt ,  180._wp )   ! who knows what's given on masked-continental regions...
      q_zu = MAX( q_zt , 1.e-6_wp )   !               "

      !! Pot. temp. difference (and we don't want it to be 0!)
      dt_zu = t_zu - T_s ;   dt_zu = SIGN( MAX(ABS(dt_zu),1.E-6_wp), dt_zu )
      dq_zu = q_zu - q_s ;   dq_zu = SIGN( MAX(ABS(dq_zu),1.E-9_wp), dq_zu )

      znu_a = visc_air(t_zu) ! Air viscosity (m^2/s) at zt given from temperature in (K)

      Ubzu = SQRT(U_zu*U_zu + 0.5_wp*0.5_wp) ! initial guess for wind gustiness contribution

      ztmp0   = LOG(    zu*10000._wp) ! optimization: 10000. == 1/z0 (with z0 first guess == 0.0001)
      ztmp1   = LOG(10._wp*10000._wp) !       "                    "               "
      u_star = 0.035_wp*Ubzu*ztmp1/ztmp0       ! (u* = 0.035*Un10)

      z0     = charn_coare3p6(U_zu)*u_star*u_star/grav + 0.11_wp*znu_a/u_star
      z0     = MIN( MAX(ABS(z0), 1.E-9) , 1._wp )                      ! (prevents FPE from stupid values from masked region later on)

      z0t    = 1._wp / ( 0.1_wp*EXP(vkarmn/(0.00115/(vkarmn/ztmp1))) )
      z0t    = MIN( MAX(ABS(z0t), 1.E-9) , 1._wp )                      ! (prevents FPE from stupid values from masked region later on)

      Cd     = MAX( (vkarmn/ztmp0)**2 , Cx_min )    ! first guess of Cd

      ztmp0 = vkarmn2/LOG(zt/z0t)/Cd

      ztmp2 = Ri_bulk( zu, T_s, t_zu, q_s, q_zu, Ubzu ) ! Bulk Richardson Number (BRN)

      !! First estimate of zeta_u, depending on the stability, ie sign of BRN (ztmp2):
      ztmp1 = 0.5 + SIGN( 0.5_wp , ztmp2 )
      zeta_u = (1._wp - ztmp1) *   ztmp0*ztmp2 / (1._wp - ztmp2*zi0*0.004_wp*Beta0**3/zu) & !  BRN < 0
         &  +       ztmp1      * ( ztmp0*ztmp2 + 27._wp/9._wp*ztmp2*ztmp2 )                 !  BRN > 0

      !! First guess M-O stability dependent scaling params.(u*,t*,q*) to estimate z0 and z/L
      ztmp0  = vkarmn/(LOG(zu/z0t) - psi_h_coare(zeta_u))

      u_star = MAX ( Ubzu*vkarmn/(LOG(zu) - LOG(z0)  - psi_m_coare(zeta_u)) , 1.E-9 )  !  (MAX => prevents FPE from stupid values from masked region later on)
      t_star = dt_zu*ztmp0
      q_star = dq_zu*ztmp0

      ! What needs to be done if zt /= zu:
      IF( .NOT. l_zt_equal_zu ) THEN
         !! First update of values at zu (or zt for wind)
         zeta_t = zt*zeta_u/zu
         ztmp0 = psi_h_coare(zeta_u) - psi_h_coare(zeta_t)
         ztmp1 = LOG(zt/zu) + ztmp0
         t_zu = t_zt - t_star/vkarmn*ztmp1
         q_zu = q_zt - q_star/vkarmn*ztmp1
         q_zu = (0.5_wp + SIGN(0.5_wp,q_zu))*q_zu !Makes it impossible to have negative humidity :
         !
         dt_zu = t_zu - T_s  ; dt_zu = SIGN( MAX(ABS(dt_zu),1.E-6_wp), dt_zu )
         dq_zu = q_zu - q_s  ; dq_zu = SIGN( MAX(ABS(dq_zu),1.E-9_wp), dq_zu )
      ENDIF

      !! ITERATION BLOCK
      DO jit = 1, nbit

         !!Inverse of Obukov length (1/L) :
         ztmp0 = One_on_L(t_zu, q_zu, u_star, t_star, q_star)  ! 1/L == 1/[Obukhov length]
         ztmp0 = SIGN( MIN(ABS(ztmp0),200._wp), ztmp0 ) ! 1/L (prevents FPE from stupid values from masked region later on...)

         ztmp1 = u_star*u_star   ! u*^2

         !! Update wind at zu with convection-related wind gustiness in unstable conditions (Fairall et al. 2003, Eq.8):
         ztmp2 = Beta0*Beta0*ztmp1*(MAX(-zi0*ztmp0/vkarmn,0._wp))**(2._wp/3._wp) ! square of wind gustiness contribution, ztmp2 == Ug^2
         !!   ! Only true when unstable (L<0) => when ztmp0 < 0 => explains "-" before zi0
         Ubzu = MAX(SQRT(U_zu*U_zu + ztmp2), 0.2_wp)        ! include gustiness in bulk wind speed
         ! => 0.2 prevents Ubzu to be 0 in stable case when U_zu=0.

         !! Stability parameters:
         zeta_u = zu*ztmp0
         zeta_u = SIGN( MIN(ABS(zeta_u),zeta_abs_max), zeta_u )
         IF( .NOT. l_zt_equal_zu ) THEN
            zeta_t = zt*ztmp0
            zeta_t = SIGN( MIN(ABS(zeta_t),zeta_abs_max), zeta_t )
         ENDIF

         !! Adjustment the wind at 10m (not needed in the current algo form):
         !IF( zu \= 10._wp ) U10 = U_zu + u_star/vkarmn*(LOG(10._wp/zu) - psi_m_coare(10._wp*ztmp0) + psi_m_coare(zeta_u))

         !! Roughness lengthes z0, z0t (z0q = z0t) :
         ztmp2 = u_star/vkarmn*LOG(10./z0)                                 ! Neutral wind speed at 10m
         z0    = charn_coare3p6(ztmp2)*ztmp1/grav + 0.11_wp*znu_a/u_star   ! Roughness length (eq.6) [ ztmp1==u*^2 ]
         z0     = MIN( MAX(ABS(z0), 1.E-9) , 1._wp )                      ! (prevents FPE from stupid values from masked region later on)

         ztmp1 = ( znu_a / (z0*u_star) )**0.72_wp     ! COARE3.6-specific! (1./Re_r)^0.72 (Re_r: roughness Reynolds number) COARE3.6-specific!
         z0t   = MIN( 1.6E-4_wp , 5.8E-5_wp*ztmp1 )   ! COARE3.6-specific!
         z0t   = MIN( MAX(ABS(z0t), 1.E-9) , 1._wp )                      ! (prevents FPE from stupid values from masked region later on)

         !! Turbulent scales at zu :
         ztmp0   = psi_h_coare(zeta_u)
         ztmp1   = vkarmn/(LOG(zu) - LOG(z0t) - ztmp0) ! #LB: in ztmp0, some use psi_h_coare(zeta_t) rather than psi_h_coare(zeta_t) ???

         t_star = dt_zu*ztmp1
         q_star = dq_zu*ztmp1
         u_star = MAX( Ubzu*vkarmn/(LOG(zu) - LOG(z0) - psi_m_coare(zeta_u)) , 1.E-9 )  !  (MAX => prevents FPE from stupid values from masked region later on)

         IF( .NOT. l_zt_equal_zu ) THEN
            !! Re-updating temperature and humidity at zu if zt /= zu :
            ztmp1 = LOG(zt/zu) + ztmp0 - psi_h_coare(zeta_t)
            t_zu = t_zt - t_star/vkarmn*ztmp1
            q_zu = q_zt - q_star/vkarmn*ztmp1
         ENDIF

         IF(( l_use_cs ).OR.( l_use_wl )) THEN
            zpre(:,:)  = pres_temp( q_zu(:,:), slp(:,:), zu, ptpot=t_zu(:,:), pta=zta(:,:) )
            zrhoa(:,:) = rho_air( zta(:,:), q_zu(:,:), zpre(:,:) )
         ENDIF

         IF( l_use_cs ) THEN
            !! Cool-skin contribution

            CALL UPDATE_QNSOL_TAU( zu, T_s, q_s, t_zu, q_zu, u_star, t_star, q_star, U_zu, Ubzu, slp, rad_lw, zrhoa, &
               &                   ztmp1, zeta_u, Qlat=ztmp2)  ! Qnsol -> ztmp1 / Tau -> zeta_u

            CALL CS_COARE( Qsw, ztmp1, u_star, zsst, ztmp2 )  ! ! Qnsol -> ztmp1 / Qlat -> ztmp2

            T_s(:,:) = zsst(:,:) + dT_cs(:,:)*tmask(:,:,1)
            IF( l_use_wl ) T_s(:,:) = T_s(:,:) + dT_wl(:,:)*tmask(:,:,1)
            q_s(:,:) = rdct_qsat_salt*q_sat(MAX(T_s(:,:), 200._wp), slp(:,:))
         ENDIF

         IF( l_use_wl ) THEN
            !! Warm-layer contribution
            CALL UPDATE_QNSOL_TAU( zu, T_s, q_s, t_zu, q_zu, u_star, t_star, q_star, U_zu, Ubzu, slp, rad_lw, zrhoa, &
               &                   ztmp1, zeta_u)  ! Qnsol -> ztmp1 / Tau -> zeta_u
            !! In WL_COARE or , Tau_ac and Qnt_ac must be updated at the final itteration step => add a flag to do this!
            CALL WL_COARE( Qsw, ztmp1, zeta_u, zsst, MOD(nbit,jit) )

            !! Updating T_s and q_s !!!
            T_s(:,:) = zsst(:,:) + dT_wl(:,:)*tmask(:,:,1)
            IF( l_use_cs ) T_s(:,:) = T_s(:,:) + dT_cs(:,:)*tmask(:,:,1)
            q_s(:,:) = rdct_qsat_salt*q_sat(MAX(T_s(:,:), 200._wp), slp(:,:))
         ENDIF

         IF( l_use_cs .OR. l_use_wl .OR. (.NOT. l_zt_equal_zu) ) THEN
            dt_zu = t_zu - T_s ;  dt_zu = SIGN( MAX(ABS(dt_zu),1.E-6_wp), dt_zu )
            dq_zu = q_zu - q_s ;  dq_zu = SIGN( MAX(ABS(dq_zu),1.E-9_wp), dq_zu )
         ENDIF

      END DO !DO jit = 1, nbit

      ! compute transfer coefficients at zu :
      ztmp0 = u_star/Ubzu
      Cd   = MAX( ztmp0*ztmp0        , Cx_min )
      Ch   = MAX( ztmp0*t_star/dt_zu , Cx_min )
      Ce   = MAX( ztmp0*q_star/dq_zu , Cx_min )

      IF( .NOT. l_zt_equal_zu ) DEALLOCATE( zeta_t )

      IF(PRESENT(Cdn)) Cdn = MAX( vkarmn2 / (LOG(zu/z0 )*LOG(zu/z0 )) , Cx_min )
      IF(PRESENT(Chn)) Chn = MAX( vkarmn2 / (LOG(zu/z0t)*LOG(zu/z0t)) , Cx_min )
      IF(PRESENT(Cen)) Cen = MAX( vkarmn2 / (LOG(zu/z0t)*LOG(zu/z0t)) , Cx_min )

      IF( l_use_cs .AND. PRESENT(pdT_cs) ) pdT_cs = dT_cs
      IF( l_use_wl .AND. PRESENT(pdT_wl) ) pdT_wl = dT_wl
      IF( l_use_wl .AND. PRESENT(pHz_wl) ) pHz_wl = Hz_wl

      IF( l_use_cs .OR. l_use_wl ) DEALLOCATE ( zsst )

   END SUBROUTINE turb_coare3p6


   FUNCTION charn_coare3p6( pwnd )
      !!-------------------------------------------------------------------
      !! Computes the Charnock parameter as a function of the Neutral wind speed at 10m
      !!  "wind speed dependent formulation"
      !!  (Eq. 13 in Edson et al., 2013)
      !!
      !! Author: L. Brodeau, July 2019 / AeroBulk  (https://github.com/brodeau/aerobulk/)
      !!-------------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj) :: charn_coare3p6
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: pwnd   ! neutral wind speed at 10m
      !
      REAL(wp), PARAMETER :: charn0_max = 0.028  !: value above which the Charnock parameter levels off for winds > 18 m/s
      !!-------------------------------------------------------------------
      charn_coare3p6 = MAX( MIN( 0.0017_wp*pwnd - 0.005_wp , charn0_max) , 0._wp )
      !!
   END FUNCTION charn_coare3p6

   FUNCTION charn_coare3p6_wave( pus, pwsh, pwps )
      !!-------------------------------------------------------------------
      !! Computes the Charnock parameter as a function of wave information and u*
      !!
      !!  (COARE 3.6, Fairall et al., 2018)
      !!
      !! Author: L. Brodeau, October 2019 / AeroBulk  (https://github.com/brodeau/aerobulk/)
      !!-------------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj) :: charn_coare3p6_wave
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: pus   ! friction velocity             [m/s]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: pwsh  ! significant wave height       [m]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: pwps  ! phase speed of dominant waves [m/s]
      !!-------------------------------------------------------------------
      charn_coare3p6_wave = ( pwsh*0.2_wp*(pus/pwps)**2.2_wp ) * grav/(pus*pus)
      !!
   END FUNCTION charn_coare3p6_wave


   FUNCTION psi_m_coare( pzeta )
      !!----------------------------------------------------------------------------------
      !! ** Purpose: compute the universal profile stability function for momentum
      !!             COARE 3.0, Fairall et al. 2003
      !!             pzeta : stability paramenter, z/L where z is altitude
      !!                     measurement and L is M-O length
      !!       Stability function for wind speed and scalars matching Kansas and free
      !!       convection forms with weighting f convective form, follows Fairall et
      !!       al (1996) with profile constants from Grachev et al (2000) BLM stable
      !!       form from Beljaars and Holtslag (1991)
      !!
      !! ** Author: L. Brodeau, June 2016 / AeroBulk (https://github.com/brodeau/aerobulk/)
      !!----------------------------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj) :: psi_m_coare
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: pzeta
      !
      INTEGER  ::   ji, jj    ! dummy loop indices
      REAL(wp) :: zta, zphi_m, zphi_c, zpsi_k, zpsi_c, zf, zc, zstab
      !!----------------------------------------------------------------------------------
      DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
            !
            zta = pzeta(ji,jj)
            !
            zphi_m = ABS(1. - 15.*zta)**.25    !!Kansas unstable
            !
            zpsi_k = 2.*LOG((1. + zphi_m)/2.) + LOG((1. + zphi_m*zphi_m)/2.)   &
               & - 2.*ATAN(zphi_m) + 0.5*rpi
            !
            zphi_c = ABS(1. - 10.15*zta)**.3333                   !!Convective
            !
            zpsi_c = 1.5*LOG((1. + zphi_c + zphi_c*zphi_c)/3.) &
               &     - 1.7320508*ATAN((1. + 2.*zphi_c)/1.7320508) + 1.813799447
            !
            zf = zta*zta
            zf = zf/(1. + zf)
            zc = MIN(50._wp, 0.35_wp*zta)
            zstab = 0.5 + SIGN(0.5_wp, zta)
            !
            psi_m_coare(ji,jj) = (1. - zstab) * ( (1. - zf)*zpsi_k + zf*zpsi_c ) & ! (zta < 0)
               &                -   zstab     * ( 1. + 1.*zta     &                ! (zta > 0)
               &                         + 0.6667*(zta - 14.28)/EXP(zc) + 8.525 )  !     "
      END_2D
   END FUNCTION psi_m_coare


   FUNCTION psi_h_coare( pzeta )
      !!---------------------------------------------------------------------
      !! Universal profile stability function for temperature and humidity
      !! COARE 3.0, Fairall et al. 2003
      !!
      !! pzeta : stability paramenter, z/L where z is altitude measurement
      !!         and L is M-O length
      !!
      !! Stability function for wind speed and scalars matching Kansas and free
      !! convection forms with weighting f convective form, follows Fairall et
      !! al (1996) with profile constants from Grachev et al (2000) BLM stable
      !! form from Beljaars and Holtslag (1991)
      !!
      !! Author: L. Brodeau, June 2016 / AeroBulk
      !!         (https://github.com/brodeau/aerobulk/)
      !!----------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj) :: psi_h_coare
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: pzeta
      !
      INTEGER  ::   ji, jj     ! dummy loop indices
      REAL(wp) :: zta, zphi_h, zphi_c, zpsi_k, zpsi_c, zf, zc, zstab
      !!----------------------------------------------------------------
      DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )
            !
            zta = pzeta(ji,jj)
            !
            zphi_h = (ABS(1. - 15.*zta))**.5  !! Kansas unstable   (zphi_h = zphi_m**2 when unstable, zphi_m when stable)
            !
            zpsi_k = 2.*LOG((1. + zphi_h)/2.)
            !
            zphi_c = (ABS(1. - 34.15*zta))**.3333   !! Convective
            !
            zpsi_c = 1.5*LOG((1. + zphi_c + zphi_c*zphi_c)/3.) &
               &    -1.7320508*ATAN((1. + 2.*zphi_c)/1.7320508) + 1.813799447
            !
            zf = zta*zta
            zf = zf/(1. + zf)
            zc = MIN(50._wp,0.35_wp*zta)
            zstab = 0.5 + SIGN(0.5_wp, zta)
            !
            psi_h_coare(ji,jj) = (1. - zstab) * ( (1. - zf)*zpsi_k + zf*zpsi_c ) &
               &                -   zstab     * ( (ABS(1. + 2.*zta/3.))**1.5     &
               &                           + .6667*(zta - 14.28)/EXP(zc) + 8.525 )
      END_2D
   END FUNCTION psi_h_coare

   !!======================================================================
END MODULE sbcblk_algo_coare3p6
