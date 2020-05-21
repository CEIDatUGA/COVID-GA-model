/* pomp C snippet file: tmp1 */
/* Time: 2020-05-20 20:09:30.388 -0400 */
/* Salt: 8A2D42ADED1C7E9E4E26165C */

#include <pomp.h>
#include <R_ext/Rdynload.h>

 


/* C snippet: 'rinit' */
#define log_beta_s		(__p[__parindex[0]])
#define trans_e		(__p[__parindex[1]])
#define trans_a		(__p[__parindex[2]])
#define trans_c		(__p[__parindex[3]])
#define trans_h		(__p[__parindex[4]])
#define log_g_e		(__p[__parindex[5]])
#define log_g_a		(__p[__parindex[6]])
#define log_g_su		(__p[__parindex[7]])
#define log_g_sd		(__p[__parindex[8]])
#define log_g_c		(__p[__parindex[9]])
#define log_g_h		(__p[__parindex[10]])
#define log_max_diag		(__p[__parindex[11]])
#define log_diag_inc_rate		(__p[__parindex[12]])
#define log_half_diag		(__p[__parindex[13]])
#define max_detect_par		(__p[__parindex[14]])
#define log_detect_inc_rate		(__p[__parindex[15]])
#define log_half_detect		(__p[__parindex[16]])
#define frac_asym		(__p[__parindex[17]])
#define frac_hosp		(__p[__parindex[18]])
#define frac_dead		(__p[__parindex[19]])
#define log_theta_cases		(__p[__parindex[20]])
#define log_theta_hosps		(__p[__parindex[21]])
#define log_theta_deaths		(__p[__parindex[22]])
#define log_sigma_dw		(__p[__parindex[23]])
#define S_0		(__p[__parindex[24]])
#define E1_0		(__p[__parindex[25]])
#define Ia1_0		(__p[__parindex[26]])
#define Isu1_0		(__p[__parindex[27]])
#define Isd1_0		(__p[__parindex[28]])
#define C1_0		(__p[__parindex[29]])
#define H1_0		(__p[__parindex[30]])
#define R_0		(__p[__parindex[31]])
#define D_0		(__p[__parindex[32]])
#define rel_beta_change		(__covars[__covindex[0]])
#define S		(__x[__stateindex[0]])
#define E1		(__x[__stateindex[1]])
#define E2		(__x[__stateindex[2]])
#define E3		(__x[__stateindex[3]])
#define E4		(__x[__stateindex[4]])
#define Ia1		(__x[__stateindex[5]])
#define Ia2		(__x[__stateindex[6]])
#define Ia3		(__x[__stateindex[7]])
#define Ia4		(__x[__stateindex[8]])
#define Isu1		(__x[__stateindex[9]])
#define Isu2		(__x[__stateindex[10]])
#define Isu3		(__x[__stateindex[11]])
#define Isu4		(__x[__stateindex[12]])
#define Isd1		(__x[__stateindex[13]])
#define Isd2		(__x[__stateindex[14]])
#define Isd3		(__x[__stateindex[15]])
#define Isd4		(__x[__stateindex[16]])
#define C1		(__x[__stateindex[17]])
#define C2		(__x[__stateindex[18]])
#define C3		(__x[__stateindex[19]])
#define C4		(__x[__stateindex[20]])
#define H1		(__x[__stateindex[21]])
#define H2		(__x[__stateindex[22]])
#define H3		(__x[__stateindex[23]])
#define H4		(__x[__stateindex[24]])
#define C_new		(__x[__stateindex[25]])
#define H_new		(__x[__stateindex[26]])
#define D_new		(__x[__stateindex[27]])
#define R		(__x[__stateindex[28]])
#define D		(__x[__stateindex[29]])

void __pomp_rinit (double *__x, const double *__p, double t, const int *__stateindex, const int *__parindex, const int *__covindex, const double *__covars)
{
 
    S = nearbyint(S_0);
    E1 = nearbyint(exp(E1_0));
    E2 = nearbyint(exp(E1_0));
    E3 = nearbyint(exp(E1_0));
    E4 = nearbyint(exp(E1_0));
    
    Ia1 = nearbyint(exp(Ia1_0));
    Ia2 = nearbyint(exp(Ia1_0));
    Ia3 = nearbyint(exp(Ia1_0));
    Ia4 = nearbyint(exp(Ia1_0));
    
    Isu1 = nearbyint(exp(Isu1_0));
    Isu2 = nearbyint(exp(Isu1_0));
    Isu3 = nearbyint(exp(Isu1_0));
    Isu4 = nearbyint(exp(Isu1_0));
  
    Isd1 = nearbyint(exp(Isd1_0));
    Isd2 = nearbyint(exp(Isd1_0));
    Isd3 = nearbyint(exp(Isd1_0));
    Isd4 = nearbyint(exp(Isd1_0));
    
    C1 = nearbyint(C1_0);
    C2 = nearbyint(C1_0);
    C3 = nearbyint(C1_0);
    C4 = nearbyint(C1_0);
    C_new = nearbyint(C1_0);
  
    H1 = nearbyint(H1_0);
    H2 = nearbyint(H1_0);
    H3 = nearbyint(H1_0);
    H4 = nearbyint(H1_0);
    H_new = nearbyint(H1_0);
  
    R = nearbyint(R_0);
    D = nearbyint(D_0);
    D_new = nearbyint(D_0);
     
}

#undef log_beta_s
#undef trans_e
#undef trans_a
#undef trans_c
#undef trans_h
#undef log_g_e
#undef log_g_a
#undef log_g_su
#undef log_g_sd
#undef log_g_c
#undef log_g_h
#undef log_max_diag
#undef log_diag_inc_rate
#undef log_half_diag
#undef max_detect_par
#undef log_detect_inc_rate
#undef log_half_detect
#undef frac_asym
#undef frac_hosp
#undef frac_dead
#undef log_theta_cases
#undef log_theta_hosps
#undef log_theta_deaths
#undef log_sigma_dw
#undef S_0
#undef E1_0
#undef Ia1_0
#undef Isu1_0
#undef Isd1_0
#undef C1_0
#undef H1_0
#undef R_0
#undef D_0
#undef rel_beta_change
#undef S
#undef E1
#undef E2
#undef E3
#undef E4
#undef Ia1
#undef Ia2
#undef Ia3
#undef Ia4
#undef Isu1
#undef Isu2
#undef Isu3
#undef Isu4
#undef Isd1
#undef Isd2
#undef Isd3
#undef Isd4
#undef C1
#undef C2
#undef C3
#undef C4
#undef H1
#undef H2
#undef H3
#undef H4
#undef C_new
#undef H_new
#undef D_new
#undef R
#undef D

/* C snippet: 'step.fn' */
#define log_beta_s		(__p[__parindex[0]])
#define trans_e		(__p[__parindex[1]])
#define trans_a		(__p[__parindex[2]])
#define trans_c		(__p[__parindex[3]])
#define trans_h		(__p[__parindex[4]])
#define log_g_e		(__p[__parindex[5]])
#define log_g_a		(__p[__parindex[6]])
#define log_g_su		(__p[__parindex[7]])
#define log_g_sd		(__p[__parindex[8]])
#define log_g_c		(__p[__parindex[9]])
#define log_g_h		(__p[__parindex[10]])
#define log_max_diag		(__p[__parindex[11]])
#define log_diag_inc_rate		(__p[__parindex[12]])
#define log_half_diag		(__p[__parindex[13]])
#define max_detect_par		(__p[__parindex[14]])
#define log_detect_inc_rate		(__p[__parindex[15]])
#define log_half_detect		(__p[__parindex[16]])
#define frac_asym		(__p[__parindex[17]])
#define frac_hosp		(__p[__parindex[18]])
#define frac_dead		(__p[__parindex[19]])
#define log_theta_cases		(__p[__parindex[20]])
#define log_theta_hosps		(__p[__parindex[21]])
#define log_theta_deaths		(__p[__parindex[22]])
#define log_sigma_dw		(__p[__parindex[23]])
#define S_0		(__p[__parindex[24]])
#define E1_0		(__p[__parindex[25]])
#define Ia1_0		(__p[__parindex[26]])
#define Isu1_0		(__p[__parindex[27]])
#define Isd1_0		(__p[__parindex[28]])
#define C1_0		(__p[__parindex[29]])
#define H1_0		(__p[__parindex[30]])
#define R_0		(__p[__parindex[31]])
#define D_0		(__p[__parindex[32]])
#define rel_beta_change		(__covars[__covindex[0]])
#define S		(__x[__stateindex[0]])
#define E1		(__x[__stateindex[1]])
#define E2		(__x[__stateindex[2]])
#define E3		(__x[__stateindex[3]])
#define E4		(__x[__stateindex[4]])
#define Ia1		(__x[__stateindex[5]])
#define Ia2		(__x[__stateindex[6]])
#define Ia3		(__x[__stateindex[7]])
#define Ia4		(__x[__stateindex[8]])
#define Isu1		(__x[__stateindex[9]])
#define Isu2		(__x[__stateindex[10]])
#define Isu3		(__x[__stateindex[11]])
#define Isu4		(__x[__stateindex[12]])
#define Isd1		(__x[__stateindex[13]])
#define Isd2		(__x[__stateindex[14]])
#define Isd3		(__x[__stateindex[15]])
#define Isd4		(__x[__stateindex[16]])
#define C1		(__x[__stateindex[17]])
#define C2		(__x[__stateindex[18]])
#define C3		(__x[__stateindex[19]])
#define C4		(__x[__stateindex[20]])
#define H1		(__x[__stateindex[21]])
#define H2		(__x[__stateindex[22]])
#define H3		(__x[__stateindex[23]])
#define H4		(__x[__stateindex[24]])
#define C_new		(__x[__stateindex[25]])
#define H_new		(__x[__stateindex[26]])
#define D_new		(__x[__stateindex[27]])
#define R		(__x[__stateindex[28]])
#define D		(__x[__stateindex[29]])

void __pomp_stepfn (double *__x, const double *__p, const int *__stateindex, const int *__parindex, const int *__covindex, const double *__covars, double t, double dt)
{
 
    // C indexes at 0, for R users making things 1 bigger and start 
    // with index 1, i.e. leave trans[0] empty, same for rate[0]
    
    double rate[15];  // 14 rates plus 1 empty
    double trans[30];  // 29 transitions plus 1 empty
    
    double E_tot, Ia_tot, Isu_tot, Isd_tot, C_tot, H_tot;
    double foi;  // force of infection
    double g_sd, g_c;  // rate of transition through I_sd compartments
    double detect_frac, diag_speedup; // fraction of those that get eventually diagnosed
    double beta;
    double dW;  // environmental stochasticity/noise
  
    E_tot = E1+E2+E3+E4;  // all pre-symptomatic
    Ia_tot = Ia1+Ia2+Ia3+Ia4;  // all asymptomatic
    Isu_tot = Isu1+Isu2+Isu3+Isu4;  // all symptomatic who will remain undiagnosed
    Isd_tot = Isd1+Isd2+Isd3+Isd4;  // all symptomatic who will become diagnosed
    C_tot = C1+C2+C3+C4;  // all diagnosed/cases
    H_tot = H1+H2+H3+H4;  // all hospitalized
    
    // compute the environmental stochasticity
    dW = rgammawn(exp(log_sigma_dw), dt);
    
    
    // ---------------------------------------------------------------
    // below we define and compute different quantities that
    // depend on time-varying interventions  
    // ---------------------------------------------------------------
    
    // Force of Infection (foi):
    // Each group can have its own transmission rate.
    // Symptomatic are assumed to transmit the most, other groups have reduced 
    //    transmission by some factor.
    // All parameters are used in the model such that they can take on any 
    //    numeric value (useful for fitting).
    // The overall foi is modulated by the unacast data stream as covariate.
    
    //foi = rel_beta_change*( pow( ( 1/(1+exp(-5.65)) ), t ) ) * (exp(log_beta_s)*(Isd_tot + Isu_tot + 1/(1+exp(trans_e))*E_tot + 1/(1+exp(trans_a))*Ia_tot + 1/(1+exp(trans_c))*C_tot+ 1/(1+exp(trans_h))*H_tot));
    foi = rel_beta_change * (exp(log_beta_s)*(Isd_tot + Isu_tot + 1/(1+exp(trans_e))*E_tot + 1/(1+exp(trans_a))*Ia_tot + 1/(1+exp(trans_c))*C_tot+ 1/(1+exp(trans_h))*H_tot));
  
    // Time-dependent rate of movement through Isd dummy compartments.
    // Starts at no speedup, then increases with time up to a max.
    // Ramp-up speed, time at which half-max is reached and max value are fitted.
    
    diag_speedup = (1 + exp(log_max_diag) ) *  pow(t, exp(log_diag_inc_rate)) / (pow(exp(log_half_diag),exp(log_diag_inc_rate))  + pow(t, exp(log_diag_inc_rate)));
    g_sd = diag_speedup*exp(log_g_sd); //shortened time in symptomatic stage prior to diagnosis
    g_c = exp(log_g_c)/diag_speedup; //increased time in symptomatic stage post diagnosis
    
    
    // Time dependent fraction of those that move into detected category at the 
    //    end of the E phase.
    // Starts at 0 at simulation start, then ramps up to some max value (0-1). 
    // Ramp-up speed and max value are fitted.
    
    detect_frac = 1/(1+exp(max_detect_par)) * pow(t, exp(log_detect_inc_rate))  / ( pow(exp(log_half_detect),exp(log_detect_inc_rate)) + pow(t,exp(log_detect_inc_rate)));
    
    
    // -----------------------------------
    // Compute the transition rates
    // -----------------------------------
    rate[1] = foi * dW/dt;  //infection, movement from S to E
    rate[2] = exp(log_g_e);  //movement through E compartments
    rate[3] = exp(log_g_e) * 1/(1+exp(frac_asym));  //from E to Ia
    rate[4] = exp(log_g_e) * (1 - 1/(1+exp(frac_asym))) * (1 - detect_frac);  //from E to Isu
    rate[5] = exp(log_g_e) * (1 - 1/(1+exp(frac_asym))) * detect_frac;  //from E to Isd
  
    rate[6] = exp(log_g_a);  //movement through Ia stages
    rate[7] = exp(log_g_su);  //movement through Isu stages
    rate[8] = g_sd;  //movement through Isd stages - computed above
    rate[9] = g_c;  //movement through C stages - computed above
    
    rate[10] = g_c * 1/(1+exp(frac_hosp));  //movement from C to H  
    rate[11] = g_c * (1 - 1/(1+exp(frac_hosp)));  //movement from C to R  
  
    rate[12] = exp(log_g_h);  //movement through H stages  
    rate[13] = exp(log_g_h) *  1/(1+exp(frac_dead));  //movement from H to D  
    rate[14] = exp(log_g_h) * (1 - 1/(1+exp(frac_dead)));  //movement from H to R  
    
    
    // ------------------------------------------
    // Compute the state transitions
    // ------------------------------------------
    // Some explanations on code, especially parts with multiple transitions.
    // reulermultinom(3, E4, &rate[3], dt, &trans[5]) says there are 3 potential 
    //    movements that come out of the E4 number of individuals, following 
    //    rates rate[3], rate[4], and rate[5]. dt is the time interval. We assign
    //    the number of transitions to trans[5], trans[6], and trans[7]. 
    //    Those trans are then used to balance the equations starting on line xxx.
    // The & symbol in C code creates automatic pointers to that iterate through 
    //    rate and trans starting at the value in the index (i) to i+(3-1).
    // m = 3, a positive integer, is number of potential transitions 
    //    (to Ia, Isu, Isd).
    // size = E4, a positive integer, is the number of individuals at 
    //    risk (pop size in E4).
    // rate is a pointer to the vector of transition (death) rates 
    //    (here rate[3:5] but in C form with the &).
    // dt, a positive real number, is the duration of time interval.
    // trans is a pointer to the vector that will hold the random deviate 
    //    (here trans[5:7] but in C form with the &).
    // See also here: https://kingaa.github.io/pomp/vignettes/C_API.html
    
    reulermultinom(1, S, &rate[1], dt, &trans[1]); // infection
    
    reulermultinom(1, E1, &rate[2], dt, &trans[2]); // move through E stages
    reulermultinom(1, E2, &rate[2], dt, &trans[3]);
    reulermultinom(1, E3, &rate[2], dt, &trans[4]);
    reulermultinom(3, E4, &rate[3], dt, &trans[5]); // move from E to Ia or Isu 
                                                    // or Isd, goes through rates 
                                                    // 3-5 and trans 5-7
    
    reulermultinom(1, Ia1, &rate[6], dt, &trans[8]); // move through Ia stages
    reulermultinom(1, Ia2, &rate[6], dt, &trans[9]);
    reulermultinom(1, Ia3, &rate[6], dt, &trans[10]);
    reulermultinom(1, Ia4, &rate[6], dt, &trans[11]); // move into R
  
    reulermultinom(1, Isu1, &rate[7], dt, &trans[12]); // move through Isu stages
    reulermultinom(1, Isu2, &rate[7], dt, &trans[13]);
    reulermultinom(1, Isu3, &rate[7], dt, &trans[14]);
    reulermultinom(1, Isu4, &rate[7], dt, &trans[15]); // move into R
  
    reulermultinom(1, Isd1, &rate[8], dt, &trans[16]); // move through Isd stages
    reulermultinom(1, Isd2, &rate[8], dt, &trans[17]);
    reulermultinom(1, Isd3, &rate[8], dt, &trans[18]);
    reulermultinom(1, Isd4, &rate[8], dt, &trans[19]); // move into C
    
    reulermultinom(1, C1, &rate[9], dt, &trans[20]); // move through C stages
    reulermultinom(1, C2, &rate[9], dt, &trans[21]);
    reulermultinom(1, C3, &rate[9], dt, &trans[22]);
    reulermultinom(2, C4, &rate[10], dt, &trans[23]); // move from C to either H 
                                                      // or R, goes through rates
                                                      // 10-11 and trans 23-24
  
  
    reulermultinom(1, H1, &rate[12], dt, &trans[25]); // move through H stages
    reulermultinom(1, H2, &rate[12], dt, &trans[26]);
    reulermultinom(1, H3, &rate[12], dt, &trans[27]);
    reulermultinom(2, H4, &rate[13], dt, &trans[28]); // move from H to either D 
                                                      // or R, goes through rates
                                                      // 13-14 and trans 28-29
  
    // ----------------------------------------------------------------
    // Apply transitions to state variables (balance equations)
    // -----------------------------------------------------------------
    S -= trans[1];
  
    E1 += trans[1] - trans[2];
    E2 += trans[2] - trans[3];
    E3 += trans[3] - trans[4];
    E4 += trans[4] - trans[5] - trans[6] - trans[7];
  
    Ia1 += trans[5] - trans[8]; //from E
    Ia2 += trans[8] - trans[9];
    Ia3 += trans[9] - trans[10];
    Ia4 += trans[10] - trans[11]; //into R
  
    Isu1 += trans[6] - trans[12]; //from E
    Isu2 += trans[12] - trans[13];
    Isu3 += trans[13] - trans[14];
    Isu4 += trans[14] - trans[15]; //into R
  
    Isd1 += trans[7] - trans[16]; //from E
    Isd2 += trans[16] - trans[17];
    Isd3 += trans[17] - trans[18];
    Isd4 += trans[18] - trans[19]; //into C
  
    C1 += trans[19] - trans[20]; //from Isd4
    C2 += trans[20] - trans[21];
    C3 += trans[21] - trans[22];
    C4 += trans[22] - trans[23] - trans[24]; //into H or R
    C_new += trans[19]; // new cases tracker, reset at obs times
  
    H1 += trans[23] - trans[25]; //from C
    H2 += trans[25] - trans[26];
    H3 += trans[26] - trans[27];
    H4 += trans[27] - trans[28] - trans[29]; //into D or R
    H_new += trans[23];  // new hosps tracker, reset at obs times
  
    R += trans[11] + trans[15] + trans[24] + trans[29];
    D += trans[28];
    D_new += trans[28];  // new deaths tracker, reset at obs times
     
}

#undef log_beta_s
#undef trans_e
#undef trans_a
#undef trans_c
#undef trans_h
#undef log_g_e
#undef log_g_a
#undef log_g_su
#undef log_g_sd
#undef log_g_c
#undef log_g_h
#undef log_max_diag
#undef log_diag_inc_rate
#undef log_half_diag
#undef max_detect_par
#undef log_detect_inc_rate
#undef log_half_detect
#undef frac_asym
#undef frac_hosp
#undef frac_dead
#undef log_theta_cases
#undef log_theta_hosps
#undef log_theta_deaths
#undef log_sigma_dw
#undef S_0
#undef E1_0
#undef Ia1_0
#undef Isu1_0
#undef Isd1_0
#undef C1_0
#undef H1_0
#undef R_0
#undef D_0
#undef rel_beta_change
#undef S
#undef E1
#undef E2
#undef E3
#undef E4
#undef Ia1
#undef Ia2
#undef Ia3
#undef Ia4
#undef Isu1
#undef Isu2
#undef Isu3
#undef Isu4
#undef Isd1
#undef Isd2
#undef Isd3
#undef Isd4
#undef C1
#undef C2
#undef C3
#undef C4
#undef H1
#undef H2
#undef H3
#undef H4
#undef C_new
#undef H_new
#undef D_new
#undef R
#undef D

/* C snippet: 'rmeasure' */
#define log_beta_s		(__p[__parindex[0]])
#define trans_e		(__p[__parindex[1]])
#define trans_a		(__p[__parindex[2]])
#define trans_c		(__p[__parindex[3]])
#define trans_h		(__p[__parindex[4]])
#define log_g_e		(__p[__parindex[5]])
#define log_g_a		(__p[__parindex[6]])
#define log_g_su		(__p[__parindex[7]])
#define log_g_sd		(__p[__parindex[8]])
#define log_g_c		(__p[__parindex[9]])
#define log_g_h		(__p[__parindex[10]])
#define log_max_diag		(__p[__parindex[11]])
#define log_diag_inc_rate		(__p[__parindex[12]])
#define log_half_diag		(__p[__parindex[13]])
#define max_detect_par		(__p[__parindex[14]])
#define log_detect_inc_rate		(__p[__parindex[15]])
#define log_half_detect		(__p[__parindex[16]])
#define frac_asym		(__p[__parindex[17]])
#define frac_hosp		(__p[__parindex[18]])
#define frac_dead		(__p[__parindex[19]])
#define log_theta_cases		(__p[__parindex[20]])
#define log_theta_hosps		(__p[__parindex[21]])
#define log_theta_deaths		(__p[__parindex[22]])
#define log_sigma_dw		(__p[__parindex[23]])
#define S_0		(__p[__parindex[24]])
#define E1_0		(__p[__parindex[25]])
#define Ia1_0		(__p[__parindex[26]])
#define Isu1_0		(__p[__parindex[27]])
#define Isd1_0		(__p[__parindex[28]])
#define C1_0		(__p[__parindex[29]])
#define H1_0		(__p[__parindex[30]])
#define R_0		(__p[__parindex[31]])
#define D_0		(__p[__parindex[32]])
#define rel_beta_change		(__covars[__covindex[0]])
#define S		(__x[__stateindex[0]])
#define E1		(__x[__stateindex[1]])
#define E2		(__x[__stateindex[2]])
#define E3		(__x[__stateindex[3]])
#define E4		(__x[__stateindex[4]])
#define Ia1		(__x[__stateindex[5]])
#define Ia2		(__x[__stateindex[6]])
#define Ia3		(__x[__stateindex[7]])
#define Ia4		(__x[__stateindex[8]])
#define Isu1		(__x[__stateindex[9]])
#define Isu2		(__x[__stateindex[10]])
#define Isu3		(__x[__stateindex[11]])
#define Isu4		(__x[__stateindex[12]])
#define Isd1		(__x[__stateindex[13]])
#define Isd2		(__x[__stateindex[14]])
#define Isd3		(__x[__stateindex[15]])
#define Isd4		(__x[__stateindex[16]])
#define C1		(__x[__stateindex[17]])
#define C2		(__x[__stateindex[18]])
#define C3		(__x[__stateindex[19]])
#define C4		(__x[__stateindex[20]])
#define H1		(__x[__stateindex[21]])
#define H2		(__x[__stateindex[22]])
#define H3		(__x[__stateindex[23]])
#define H4		(__x[__stateindex[24]])
#define C_new		(__x[__stateindex[25]])
#define H_new		(__x[__stateindex[26]])
#define D_new		(__x[__stateindex[27]])
#define R		(__x[__stateindex[28]])
#define D		(__x[__stateindex[29]])
#define cases		(__y[__obsindex[0]])
#define hosps		(__y[__obsindex[1]])
#define deaths		(__y[__obsindex[2]])

void __pomp_rmeasure (double *__y, const double *__x, const double *__p, const int *__obsindex, const int *__stateindex, const int *__parindex, const int *__covindex, const double *__covars, double t)
{
 
    double theta1, theta2, theta3;
    theta1 = exp(log_theta_cases);
    theta2 = exp(log_theta_hosps);
    theta3 = exp(log_theta_deaths);
    cases = rnbinom_mu(theta1, C_new);  // for forecasting 
    hosps = rnbinom_mu(theta2, H_new);  // for forecasting
    deaths = rnbinom_mu(theta3, D_new);  // for forecasting
     
}

#undef log_beta_s
#undef trans_e
#undef trans_a
#undef trans_c
#undef trans_h
#undef log_g_e
#undef log_g_a
#undef log_g_su
#undef log_g_sd
#undef log_g_c
#undef log_g_h
#undef log_max_diag
#undef log_diag_inc_rate
#undef log_half_diag
#undef max_detect_par
#undef log_detect_inc_rate
#undef log_half_detect
#undef frac_asym
#undef frac_hosp
#undef frac_dead
#undef log_theta_cases
#undef log_theta_hosps
#undef log_theta_deaths
#undef log_sigma_dw
#undef S_0
#undef E1_0
#undef Ia1_0
#undef Isu1_0
#undef Isd1_0
#undef C1_0
#undef H1_0
#undef R_0
#undef D_0
#undef rel_beta_change
#undef S
#undef E1
#undef E2
#undef E3
#undef E4
#undef Ia1
#undef Ia2
#undef Ia3
#undef Ia4
#undef Isu1
#undef Isu2
#undef Isu3
#undef Isu4
#undef Isd1
#undef Isd2
#undef Isd3
#undef Isd4
#undef C1
#undef C2
#undef C3
#undef C4
#undef H1
#undef H2
#undef H3
#undef H4
#undef C_new
#undef H_new
#undef D_new
#undef R
#undef D
#undef cases
#undef hosps
#undef deaths

/* C snippet: 'dmeasure' */
#define log_beta_s		(__p[__parindex[0]])
#define trans_e		(__p[__parindex[1]])
#define trans_a		(__p[__parindex[2]])
#define trans_c		(__p[__parindex[3]])
#define trans_h		(__p[__parindex[4]])
#define log_g_e		(__p[__parindex[5]])
#define log_g_a		(__p[__parindex[6]])
#define log_g_su		(__p[__parindex[7]])
#define log_g_sd		(__p[__parindex[8]])
#define log_g_c		(__p[__parindex[9]])
#define log_g_h		(__p[__parindex[10]])
#define log_max_diag		(__p[__parindex[11]])
#define log_diag_inc_rate		(__p[__parindex[12]])
#define log_half_diag		(__p[__parindex[13]])
#define max_detect_par		(__p[__parindex[14]])
#define log_detect_inc_rate		(__p[__parindex[15]])
#define log_half_detect		(__p[__parindex[16]])
#define frac_asym		(__p[__parindex[17]])
#define frac_hosp		(__p[__parindex[18]])
#define frac_dead		(__p[__parindex[19]])
#define log_theta_cases		(__p[__parindex[20]])
#define log_theta_hosps		(__p[__parindex[21]])
#define log_theta_deaths		(__p[__parindex[22]])
#define log_sigma_dw		(__p[__parindex[23]])
#define S_0		(__p[__parindex[24]])
#define E1_0		(__p[__parindex[25]])
#define Ia1_0		(__p[__parindex[26]])
#define Isu1_0		(__p[__parindex[27]])
#define Isd1_0		(__p[__parindex[28]])
#define C1_0		(__p[__parindex[29]])
#define H1_0		(__p[__parindex[30]])
#define R_0		(__p[__parindex[31]])
#define D_0		(__p[__parindex[32]])
#define rel_beta_change		(__covars[__covindex[0]])
#define S		(__x[__stateindex[0]])
#define E1		(__x[__stateindex[1]])
#define E2		(__x[__stateindex[2]])
#define E3		(__x[__stateindex[3]])
#define E4		(__x[__stateindex[4]])
#define Ia1		(__x[__stateindex[5]])
#define Ia2		(__x[__stateindex[6]])
#define Ia3		(__x[__stateindex[7]])
#define Ia4		(__x[__stateindex[8]])
#define Isu1		(__x[__stateindex[9]])
#define Isu2		(__x[__stateindex[10]])
#define Isu3		(__x[__stateindex[11]])
#define Isu4		(__x[__stateindex[12]])
#define Isd1		(__x[__stateindex[13]])
#define Isd2		(__x[__stateindex[14]])
#define Isd3		(__x[__stateindex[15]])
#define Isd4		(__x[__stateindex[16]])
#define C1		(__x[__stateindex[17]])
#define C2		(__x[__stateindex[18]])
#define C3		(__x[__stateindex[19]])
#define C4		(__x[__stateindex[20]])
#define H1		(__x[__stateindex[21]])
#define H2		(__x[__stateindex[22]])
#define H3		(__x[__stateindex[23]])
#define H4		(__x[__stateindex[24]])
#define C_new		(__x[__stateindex[25]])
#define H_new		(__x[__stateindex[26]])
#define D_new		(__x[__stateindex[27]])
#define R		(__x[__stateindex[28]])
#define D		(__x[__stateindex[29]])
#define cases		(__y[__obsindex[0]])
#define hosps		(__y[__obsindex[1]])
#define deaths		(__y[__obsindex[2]])
#define lik		(__lik[0])

void __pomp_dmeasure (double *__lik, const double *__y, const double *__x, const double *__p, int give_log, const int *__obsindex, const int *__stateindex, const int *__parindex, const int *__covindex, const double *__covars, double t)
{
 
    double d1, d2, d3;
    double theta1, theta2, theta3;
    theta1 = exp(log_theta_cases);
    theta2 = exp(log_theta_hosps);
    theta3 = exp(log_theta_deaths);
    
    if(ISNA(cases)) {
      d1 = 0;  // loglik is 0 if no observations
    } else {
      d1 = dnbinom_mu(cases, theta1, C_new, 1); 
    }
    
    if(ISNA(hosps)) {
      d2 = 0;  // loglik is 0 if no observations
    } else {
      d2 = dnbinom_mu(hosps, theta2, H_new, 1);
    }
    
    if(ISNA(deaths)) {
      d3 = 0;  // loglik is 0 if no observations
    } else {
      d3 = dnbinom_mu(deaths, theta3, D_new, 1);
    }
    
    lik = d1 + d2 + d3;  // sum the individual likelihoods
    lik = (give_log) ? lik : exp(lik);  // return loglik or exp(lik)
     
}

#undef log_beta_s
#undef trans_e
#undef trans_a
#undef trans_c
#undef trans_h
#undef log_g_e
#undef log_g_a
#undef log_g_su
#undef log_g_sd
#undef log_g_c
#undef log_g_h
#undef log_max_diag
#undef log_diag_inc_rate
#undef log_half_diag
#undef max_detect_par
#undef log_detect_inc_rate
#undef log_half_detect
#undef frac_asym
#undef frac_hosp
#undef frac_dead
#undef log_theta_cases
#undef log_theta_hosps
#undef log_theta_deaths
#undef log_sigma_dw
#undef S_0
#undef E1_0
#undef Ia1_0
#undef Isu1_0
#undef Isd1_0
#undef C1_0
#undef H1_0
#undef R_0
#undef D_0
#undef rel_beta_change
#undef S
#undef E1
#undef E2
#undef E3
#undef E4
#undef Ia1
#undef Ia2
#undef Ia3
#undef Ia4
#undef Isu1
#undef Isu2
#undef Isu3
#undef Isu4
#undef Isd1
#undef Isd2
#undef Isd3
#undef Isd4
#undef C1
#undef C2
#undef C3
#undef C4
#undef H1
#undef H2
#undef H3
#undef H4
#undef C_new
#undef H_new
#undef D_new
#undef R
#undef D
#undef cases
#undef hosps
#undef deaths
#undef lik

static int __pomp_load_stack = 0;

void __pomp_load_stack_incr (void) {++__pomp_load_stack;}

void __pomp_load_stack_decr (int *val) {*val = --__pomp_load_stack;}

void R_init_tmp1 (DllInfo *info)
{
R_RegisterCCallable("tmp1", "__pomp_load_stack_incr", (DL_FUNC) __pomp_load_stack_incr);
R_RegisterCCallable("tmp1", "__pomp_load_stack_decr", (DL_FUNC) __pomp_load_stack_decr);
R_RegisterCCallable("tmp1", "__pomp_rinit", (DL_FUNC) __pomp_rinit);
R_RegisterCCallable("tmp1", "__pomp_stepfn", (DL_FUNC) __pomp_stepfn);
R_RegisterCCallable("tmp1", "__pomp_rmeasure", (DL_FUNC) __pomp_rmeasure);
R_RegisterCCallable("tmp1", "__pomp_dmeasure", (DL_FUNC) __pomp_dmeasure);
}
