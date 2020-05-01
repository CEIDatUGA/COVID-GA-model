/* pomp C snippet file: pomp_189503c641948ba68c64d95a298cbf11 */
/* Time: 2020-05-01 08:43:51.939 -0600 */
/* Salt: 3E4F460D4AB0A3A5ED424E68 */

#include <C:/Users/atredennick/Documents/R/win-library/3.6/pomp/include/pomp.h>
#include <R_ext/Rdynload.h>

 


/* C snippet: 'dprior' */
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
#define lik		(__lik[0])

void __pomp_dprior (double *__lik, const double *__p, int give_log, const int *__parindex)
{
 lik = dnorm(log_beta_s, -16.9748882929073, 3, 1) + dnorm(frac_hosp, 2, 3, 1) + dnorm(frac_dead, 1.2, 3, 1) + dnorm(max_detect_par, 0, 3, 1) + dnorm(log_sigma_dw, -2.30258509299405, 3, 1) + dnorm(log_theta_cases, 2.30258509299405, 3, 1) + dnorm(log_theta_hosps, 2.30258509299405, 3, 1) + dnorm(log_theta_deaths, 2.30258509299405, 3, 1) + dnorm(, NA, 1, 1) ; 
 if (!give_log) lik = exp(lik); 
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
#undef lik

static int __pomp_load_stack = 0;

void __pomp_load_stack_incr (void) {++__pomp_load_stack;}

void __pomp_load_stack_decr (int *val) {*val = --__pomp_load_stack;}

void R_init_pomp_189503c641948ba68c64d95a298cbf11 (DllInfo *info)
{
R_RegisterCCallable("pomp_189503c641948ba68c64d95a298cbf11", "__pomp_load_stack_incr", (DL_FUNC) __pomp_load_stack_incr);
R_RegisterCCallable("pomp_189503c641948ba68c64d95a298cbf11", "__pomp_load_stack_decr", (DL_FUNC) __pomp_load_stack_decr);
R_RegisterCCallable("pomp_189503c641948ba68c64d95a298cbf11", "__pomp_dprior", (DL_FUNC) __pomp_dprior);
}
