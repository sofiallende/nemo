MODULE sbcblk_skin_ecmwf
   !!======================================================================
   !!                   ***  MODULE  sbcblk_skin_ecmwf  ***
   !!
   !!   Module that gathers the cool-skin and warm-layer parameterization used
   !!   by the IFS at ECMWF (recoded from scratch =>
   !!   https://github.com/brodeau/aerobulk)
   !!
   !!  Mainly based on Zeng & Beljaars, 2005 with the more recent add-up from
   !!  Takaya et al., 2010 when it comes to the warm-layer parameterization
   !!  (contribution of extra mixing due to Langmuir circulation)
   !!
   !!  - Zeng X., and A. Beljaars, 2005: A prognostic scheme of sea surface skin
   !!    temperature for modeling and data assimilation. Geophysical Research
   !!    Letters, 32 (14) , pp. 1-4.
   !!
   !!  - Takaya, Y., J.-R. Bildot, A. C. M. Beljaars, and P. A. E. M. Janssen,
   !!    2010: Refinements to a prognostic scheme of skin sea surface
   !!    temperature. J. Geophys. Res., 115, C06009, doi:10.1029/2009JC005985
   !!
   !!   Most of the formula are taken from the documentation of IFS of ECMWF
   !!            (cycle 40r1) (avaible online on the ECMWF's website)
   !!
   !!   Routine 'sbcblk_skin_ecmwf' also maintained and developed in AeroBulk (as
   !!            'mod_skin_ecmwf')
   !!    (https://github.com/brodeau/aerobulk)
   !!
   !! ** Author: L. Brodeau, November 2019 / AeroBulk (https://github.com/brodeau/aerobulk)
   !!----------------------------------------------------------------------
   !! History :  4.0  ! 2019-11  (L.Brodeau)   Original code
   !!            4.2  ! 2020-12  (L. Brodeau) Introduction of various air-ice bulk parameterizations + improvements
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and tracers
   USE dom_oce         ! ocean space and time domain
   USE phycst          ! physical constants
   USE sbc_oce         ! Surface boundary condition: ocean fields

   USE sbc_phy         ! Catalog of functions for physical/meteorological parameters in the marine boundary layer

   USE lib_mpp         ! distribued memory computing library
   USE in_out_manager  ! I/O manager
   USE lib_fortran     ! to use key_nosignedzero

   IMPLICIT NONE
   PRIVATE

   PUBLIC :: CS_ECMWF, WL_ECMWF
   !! * Substitutions
#  include "do_loop_substitute.h90"

   !! Cool-skin related parameters:
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:), PUBLIC :: dT_cs !: dT due to cool-skin effect
   !                                                      ! => temperature difference between air-sea interface (z=0)
   !                                                      !    and right below viscous layer (z=delta)

   !! Warm-layer related parameters:
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:), PUBLIC :: dT_wl !: dT due to warm-layer effect
   !                                                      ! => difference between "almost surface (right below
   !                                                      !    viscous layer, z=delta) and depth of bulk SST (z=gdept_1d(1))
   REAL(wp), ALLOCATABLE, SAVE, DIMENSION(:,:), PUBLIC :: Hz_wl !: depth (aka thickness) of warm-layer [m]
   !
   REAL(wp), PARAMETER, PUBLIC :: rd0  = 3.    !: Depth scale [m] of warm layer, "d" in Eq.11 (Zeng & Beljaars 2005)
   REAL(wp), PARAMETER         :: zRhoCp_w = rho0_w*rCp0_w
   !
   REAL(wp), PARAMETER         :: rNuwl0 = 0.5  !: Nu (exponent of temperature profile) Eq.11
   !                                            !: (Zeng & Beljaars 2005) !: set to 0.5 instead of
   !                                            !: 0.3 to respect a warming of +3 K in calm
   !                                            !: condition for the insolation peak of +1000W/m^2
   !!----------------------------------------------------------------------
CONTAINS


   SUBROUTINE CS_ECMWF( pQsw, pQnsol, pustar, pSST )
      !!---------------------------------------------------------------------
      !!
      !! Cool-skin parameterization, based on Fairall et al., 1996:
      !!
      !!  - Zeng X., and A. Beljaars, 2005: A prognostic scheme of sea surface
      !!    skin temperature for modeling and data assimilation. Geophysical
      !!    Research Letters, 32 (14) , pp. 1-4.
      !!
      !!------------------------------------------------------------------
      !!
      !!  **   INPUT:
      !!     *pQsw*       surface net solar radiation into the ocean     [W/m^2] => >= 0 !
      !!     *pQnsol*     surface net non-solar heat flux into the ocean [W/m^2] => normally < 0 !
      !!     *pustar*     friction velocity u*                           [m/s]
      !!     *pSST*       bulk SST (taken at depth gdept_1d(1))          [K]
      !!------------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: pQsw   ! net solar a.k.a shortwave radiation into the ocean (after albedo) [W/m^2]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: pQnsol ! non-solar heat flux to the ocean [W/m^2]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: pustar  ! friction velocity, temperature and humidity (u*,t*,q*)
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: pSST ! bulk SST [K]
      !!---------------------------------------------------------------------
      INTEGER  :: ji, jj, jc
      REAL(wp) :: zQabs, zdlt, zfr, zalfa, zus
      !!---------------------------------------------------------------------
      DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )

         zQabs = pQnsol(ji,jj) ! first guess of heat flux absorbed within the viscous sublayer of thicknes delta,
         !                     !   => we DO not miss a lot assuming 0 solar flux absorbed in the tiny layer of thicknes zdlt...

         zalfa = alpha_sw(pSST(ji,jj)) ! (crude) thermal expansion coefficient of sea-water [1/K]
         zus   = pustar(ji,jj)

         zdlt = delta_skin_layer( zalfa, zQabs, zus )

         DO jc = 1, 4 ! because implicit in terms of zdlt...
            zfr = MAX( 0.065_wp + 11._wp*zdlt  &
               &       - 6.6E-5_wp/zdlt*(1._wp - EXP(-zdlt/8.E-4_wp)) &
               &      , 0.01_wp ) ! Solar absorption, Eq.(5) Zeng & Beljaars, 2005
            !                     !  =>  (WARNING: 0.065 rather than 0.137 in Fairal et al. 1996)
            zQabs = pQnsol(ji,jj) + zfr*pQsw(ji,jj)
            zdlt = delta_skin_layer( zalfa, zQabs, zus )
         END DO

         dT_cs(ji,jj) = zQabs*zdlt/rk0_w   ! temperature increment, yes dT_cs can actually > 0, if Qabs > 0 (rare but possible!)

      END_2D

   END SUBROUTINE CS_ECMWF


   SUBROUTINE WL_ECMWF( pQsw, pQnsol, pustar, pSST,  pustk )
      !!---------------------------------------------------------------------
      !!
      !!  Warm-Layer scheme according to Zeng & Beljaars, 2005 (GRL) with the
      !!  more recent add-up from Takaya et al., 2010 when it comes to the
      !!  warm-layer parameterization (contribution of extra mixing due to
      !!  Langmuir circulation)
      !!
      !!  - Zeng X., and A. Beljaars, 2005: A prognostic scheme of sea surface skin
      !!    temperature for modeling and data assimilation. Geophysical Research
      !!    Letters, 32 (14) , pp. 1-4.
      !!
      !!  - Takaya, Y., J.-R. Bildot, A. C. M. Beljaars, and P. A. E. M. Janssen,
      !!    2010: Refinements to a prognostic scheme of skin sea surface
      !!    temperature. J. Geophys. Res., 115, C06009, doi:10.1029/2009JC005985
      !!
      !!  STIL NO PROGNOSTIC EQUATION FOR THE DEPTH OF THE WARM-LAYER!
      !!
      !!     ------------------------------------------------------------------
      !!
      !!  **   INPUT:
      !!     *pQsw*       surface net solar radiation into the ocean     [W/m^2] => >= 0 !
      !!     *pQnsol*     surface net non-solar heat flux into the ocean [W/m^2] => normally < 0 !
      !!     *pustar*     friction velocity u*                           [m/s]
      !!     *pSST*       bulk SST  (taken at depth gdept_1d(1))         [K]
      !!------------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in)  :: pQsw     ! surface net solar radiation into the ocean [W/m^2]     => >= 0 !
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in)  :: pQnsol   ! surface net non-solar heat flux into the ocean [W/m^2] => normally < 0 !
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in)  :: pustar   ! friction velocity [m/s]
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in)  :: pSST     ! bulk SST at depth gdept_1d(1) [K]
      !!
      REAL(wp), DIMENSION(jpi,jpj), OPTIONAL, INTENT(in) :: pustk ! surface Stokes velocity [m/s]
      !
      INTEGER :: ji, jj, jc
      !
      REAL(wp) :: zHwl      !: thickness of the warm-layer [m]
      REAL(wp) :: ztcorr    !: correction of dT w.r.t measurement depth of bulk SST (first T-point)
      REAL(wp) :: zalfa     !: thermal expansion coefficient of sea-water [1/K]
      REAL(wp) :: zdTwl_b, zdTwl_n  !: temp. diff. between "almost surface (right below viscous layer) and bottom of WL
      REAL(wp) :: zfr, zeta
      REAL(wp) :: zusw, zusw2
      REAL(wp) :: zLa, zfLa
      REAL(wp) :: flg, zwf, zQabs
      REAL(wp) :: ZA, ZB, zL1, zL2
      REAL(wp) :: zcst0, zcst1, zcst2, zcst3
      !
      LOGICAL :: l_pustk_known
      !!---------------------------------------------------------------------

      l_pustk_known = .FALSE.
      IF( PRESENT(pustk) ) l_pustk_known = .TRUE.

      DO_2D( nn_hls, nn_hls, nn_hls, nn_hls )

         zHwl = Hz_wl(ji,jj) ! first guess for warm-layer depth (and unique..., less advanced than COARE3p6 !)
         ! it is = rd0 (3m) in default Zeng & Beljaars case...

         !! Previous value of dT / warm-layer, adapted to depth:
         flg = 0.5_wp + SIGN( 0.5_wp , gdept_1d(1)-zHwl ) ! => 1 when gdept_1d(1)>zHwl (dT_wl(ji,jj) = zdTwl) | 0 when z_s$
         ztcorr = flg + (1._wp - flg)*gdept_1d(1)/zHwl
         zdTwl_b = MAX ( dT_wl(ji,jj) / ztcorr , 0._wp )
         ! zdTwl is the difference between "almost surface (right below viscous layer) and bottom of WL (here zHwl)
         ! pdT         "                          "                                    and depth of bulk SST (here gdept_1d(1))!
         !! => but of course in general the bulk SST is taken shallower than zHwl !!! So correction less pronounced!
         !! => so here since pdT is difference between surface and gdept_1d(1), need to increase fof zdTwl !

         zalfa = alpha_sw( pSST(ji,jj) ) ! (crude) thermal expansion coefficient of sea-water [1/K] (SST accurate enough!)

         ! *** zfr = Fraction of solar radiation absorbed in warm layer (-)
         zfr = 1._wp - 0.28_wp*EXP(-71.5_wp*zHwl) - 0.27_wp*EXP(-2.8_wp*zHwl) - 0.45_wp*EXP(-0.07_wp*zHwl)  !: Eq. 8.157

         zQabs = zfr*pQsw(ji,jj) + pQnsol(ji,jj)       ! tot heat absorbed in warm layer

         zusw  = MAX( pustar(ji,jj), 1.E-4_wp ) * sq_radrw    ! u* in the water
         zusw2 = zusw*zusw

         ! Langmuir:
         IF( l_pustk_known ) THEN
            zLa = SQRT(zusw/MAX(pustk(ji,jj),1.E-6))
         ELSE
            zla = 0.3_wp
         ENDIF
         zfLa = MAX( zla**(-2._wp/3._wp) , 1._wp )   ! Eq.(6)

         zwf = 0.5_wp + SIGN(0.5_wp, zQabs)  ! zQabs > 0. => 1.  / zQabs < 0. => 0.

         zcst1 = vkarmn*grav*zalfa

         ! 1/L when zQabs > 0 :
         zL2 = zcst1*zQabs / (zRhoCp_w*zusw2*zusw)

         zcst2 = zcst1 / ( 5._wp*zHwl*zusw2 )  !OR: zcst2 = zcst1*rNuwl0 / ( 5._wp*zHwl*zusw2 ) ???

         zcst0 = rn_Dt * (rNuwl0 + 1._wp) / zHwl

         ZA = zcst0 * zQabs / ( rNuwl0 * zRhoCp_w )

         zcst3 = -zcst0 * vkarmn * zusw * zfLa

         !! Sorry about all these constants ( constant w.r.t zdTwl), it's for
         !! the sake of optimizations... So all these operations are not done
         !! over and over within the iteration loop...

         !! T R U L L Y   I M P L I C I T => needs itteration
         !! => have to itterate just because the 1/(Monin-Obukhov length), zL1, uses zdTwl when zQabs < 0..
         !!    (without this term otherwize the implicit analytical solution is straightforward...)
         zdTwl_n = zdTwl_b
         DO jc = 1, 10

            zdTwl_n = 0.5_wp * ( zdTwl_n + zdTwl_b ) ! semi implicit, for faster convergence

            ! 1/L when zdTwl > 0 .AND. zQabs < 0 :
            zL1 =         SQRT( zdTwl_n * zcst2 ) ! / zusw !!! Or??? => vkarmn * SQRT( zdTwl_n*grav*zalfa/( 5._wp*zHwl ) ) / zusw

            ! Stability parameter (z/L):
            zeta =  (1._wp - zwf) * zHwl*zL1   +   zwf * zHwl*zL2

            ZB = zcst3 / PHI(zeta)

            zdTwl_n = MAX ( zdTwl_b + ZA + ZB*zdTwl_n , 0._wp )  ! Eq.(6)

         END DO

         !! Update:
         dT_wl(ji,jj) = zdTwl_n * ztcorr

      END_2D

   END SUBROUTINE WL_ECMWF


   FUNCTION delta_skin_layer( palpha, pQd, pustar_a )
      !!---------------------------------------------------------------------
      !! Computes the thickness (m) of the viscous skin layer.
      !! Based on Fairall et al., 1996
      !!
      !! Fairall, C. W., Bradley, E. F., Godfrey, J. S., Wick, G. A.,
      !! Edson, J. B., and Young, G. S. ( 1996), Cool‐skin and warm‐layer
      !! effects on sea surface temperature, J. Geophys. Res., 101( C1), 1295-1308,
      !! doi:10.1029/95JC03190.
      !!
      !! L. Brodeau, october 2019
      !!---------------------------------------------------------------------
      REAL(wp)                :: delta_skin_layer
      REAL(wp), INTENT(in)    :: palpha   ! thermal expansion coefficient of sea-water (SST accurate enough!)
      REAL(wp), INTENT(in)    :: pQd ! < 0 !!! part of the net heat flux actually absorbed in the WL [W/m^2]
      !                              !  => term "Q + Rs*fs" in eq.6 of Fairall et al. 1996
      REAL(wp), INTENT(in)    :: pustar_a ! friction velocity in the air (u*) [m/s]
      !!---------------------------------------------------------------------
      REAL(wp) :: zusw, zusw2, zlamb, ztf, ztmp
      !!---------------------------------------------------------------------
      ztf = 0.5_wp + SIGN(0.5_wp, pQd)  ! Qabs < 0 => cooling of the viscous layer => ztf = 0 (regular case)
      !                                 ! Qabs > 0 => warming of the viscous layer => ztf = 1
      !                                 !    (ex: weak evaporation and strong positive sensible heat flux)
      zusw  = MAX(pustar_a, 1.E-4_wp) * sq_radrw    ! u* in the water
      zusw2 = zusw*zusw
      !
      zlamb = 6._wp*( 1._wp + MAX(palpha*rcst_cs/(zusw2*zusw2)*pQd, 0._wp)**0.75 )**(-1./3.) ! see Eq.(14) in Fairall et al., 1996
      !  => zlamb is not used when Qd > 0, and since rcst_cs < 0, we just use this "MAX" to prevent FPE errors (something_negative)**0.75
      !
      ztmp = rnu0_w/zusw
      delta_skin_layer = (1._wp-ztf) *     zlamb*ztmp           &  ! regular case, Qd < 0, see Eq.(12) in Fairall et al., 1996
         &               +   ztf     * MIN(6._wp*ztmp , 0.007_wp)  ! when Qd > 0
   END FUNCTION delta_skin_layer


   FUNCTION PHI( pzeta)
      !!---------------------------------------------------------------------
      !!
      !! Takaya et al., 2010
      !!  Eq.(5)
      !! L. Brodeau, october 2019
      !!---------------------------------------------------------------------
      REAL(wp)                :: PHI
      REAL(wp), INTENT(in)    :: pzeta    ! stability parameter
      !!---------------------------------------------------------------------
      REAL(wp) :: ztf, zzt2
      !!---------------------------------------------------------------------
      zzt2 = pzeta*pzeta
      ztf = 0.5_wp + SIGN(0.5_wp, pzeta)  ! zeta > 0 => ztf = 1
      !                                   ! zeta < 0 => ztf = 0
      PHI =      ztf     * ( 1. + (5.*pzeta + 4.*zzt2)/(1. + 3.*pzeta + 0.25*zzt2) ) &   ! zeta > 0
         &  + (1. - ztf) * 1./SQRT( 1. - 16.*(-ABS(pzeta)) )                             ! zeta < 0
   END FUNCTION PHI

   !!======================================================================
END MODULE sbcblk_skin_ecmwf
