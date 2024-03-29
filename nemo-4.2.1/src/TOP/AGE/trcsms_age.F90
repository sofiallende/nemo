MODULE trcsms_age
   !!======================================================================
   !!                         ***  MODULE trcsms_age  ***
   !! TOP :   Main module of the AGE tracers
   !!======================================================================
   !! History :   2.0  !  2007-12  (C. Ethe, G. Madec) Original code
   !!----------------------------------------------------------------------
   !! trc_sms_age       : AGE model main routine
   !!----------------------------------------------------------------------
   USE oce_trc         ! Ocean variables
   USE trc             ! TOP variables
   USE trd_oce
   USE trdtrc

   IMPLICIT NONE
   PRIVATE

   PUBLIC   trc_sms_age       ! called by trcsms.F90 module

   INTEGER , PUBLIC :: nl_age             ! T level surrounding age_depth
   INTEGER , PUBLIC :: nla_age            ! T level wholly above age_depth
   INTEGER , PUBLIC :: nlb_age            ! T level wholly below age_depth

   REAL(wp), PUBLIC :: rn_age_depth       ! = 10       depth over which age tracer reset to zero
   REAL(wp), PUBLIC :: rn_age_kill_rate   ! = -1./7200  recip of relaxation timescale (s) for  age tracer shallower than age_depth
   
   REAL(wp), PUBLIC :: rryear          !: recip number of seconds in one year
   REAL(wp), PUBLIC :: frac_kill_age   !: fraction of level nl_age above age_depth where it is relaxed towards zero
   REAL(wp), PUBLIC :: frac_add_age    !: fraction of level nl_age below age_depth where it is incremented


   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: trcsms_age.F90 14173 2020-12-15 12:44:07Z cetlod $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE trc_sms_age( kt, Kbb, Kmm, Krhs )
      !!----------------------------------------------------------------------
      !!                     ***  trc_sms_age  ***
      !!
      !! ** Purpose :   main routine of AGE model
      !!
      !! ** Method  : -
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt              ! ocean time-step index
      INTEGER, INTENT(in) ::   Kbb, Kmm, Krhs  ! ocean time level
      INTEGER ::   jn, jk   ! dummy loop index
      !!----------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('trc_sms_age')
      !
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) ' trc_sms_age:  AGE model'
      IF(lwp) WRITE(numout,*) ' ~~~~~~~~~~~~~~'
      !
      DO jk = 1, nla_age
         tr(:,:,jk,jp_age,Krhs) = rn_age_kill_rate * tr(:,:,jk,jp_age,Kbb)
      END DO
      !
      tr(:,:,nl_age,jp_age,Krhs) = frac_kill_age * rn_age_kill_rate * tr(:,:,nl_age,jp_age,Kbb)  &
          &                   + frac_add_age  * rryear * tmask(:,:,nl_age)
      !
      DO jk = nlb_age, jpk
         tr(:,:,jk,jp_age,Krhs) = tmask(:,:,jk) * rryear
      END DO
      !
      IF( l_trdtrc ) CALL trd_trc( tr(:,:,:,jp_age,Krhs), jn, jptra_sms, kt, Kmm )   ! save trends
      !
      IF( ln_timing )   CALL timing_stop('trc_sms_age')
      !
   END SUBROUTINE trc_sms_age

   !!======================================================================
END MODULE trcsms_age
