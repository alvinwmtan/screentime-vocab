mod_str <- '
# =============================================================================
# STRUCTURAL MODEL WITH MODERATION EFFECTS
# =============================================================================

# outcome with moderation effects
centile ~ z1*1 + c1*age + c2*gender + 
  c3L*ses.L + c3Q*ses.Q + c3C*ses.C + c3_4*ses.4 + c3_5*ses.5 +
  c4*childcare_halfdays + c5*num_older_siblings + 
  b11*motivation_behmgmt + b12*motivation_freeing + 
  b13*motivation_learning + b14*motivation_emoreg + 
  b21*interaction_expanding + b22*interaction_focusing + 
  b23*interaction_asking + b24*supervision_accompanied + 
  b25*screentime_weekly +
  b26*exp_time + b27*foc_time + b28*ask_time + b29*acc_time +
  b31*child_beh + b32*child_free + b33*child_learn + b34*child_emo +
  b35*child_exp + b36*child_foc + b37*child_ask + 
  b38*child_acc + b39*child_time +
  b41*sib_beh + b42*sib_free + b43*sib_learn + b44*sib_emo +
  b45*sib_exp + b46*sib_foc + b47*sib_ask +
  b48*sib_acc + b49*sib_time

# level 2 mediators
interaction_expanding ~ z21*1 + d11*age + e11*gender + 
  f11L*ses.L + f11Q*ses.Q + f11C*ses.C + f11_4*ses.4 + f11_5*ses.5 +
  g11*childcare_halfdays + h11*num_older_siblings + 
  a211*motivation_behmgmt + a212*motivation_freeing + 
  a213*motivation_learning + a214*motivation_emoreg

interaction_focusing ~ z22*1 + d12*age + e12*gender + 
  f12L*ses.L + f12Q*ses.Q + f12C*ses.C + f12_4*ses.4 + f12_5*ses.5 +
  g12*childcare_halfdays + h12*num_older_siblings + 
  a221*motivation_behmgmt + a222*motivation_freeing + 
  a223*motivation_learning + a224*motivation_emoreg

interaction_asking ~ z23*1 + d13*age + e13*gender + 
  f13L*ses.L + f13Q*ses.Q + f13C*ses.C + f13_4*ses.4 + f13_5*ses.5 +
  g13*childcare_halfdays + h13*num_older_siblings + 
  a231*motivation_behmgmt + a232*motivation_freeing + 
  a233*motivation_learning + a234*motivation_emoreg

supervision_accompanied ~ d14*age + e14*gender + 
  f14L*ses.L + f14Q*ses.Q + f14C*ses.C + f14_4*ses.4 + f14_5*ses.5 +
  g14*childcare_halfdays + h14*num_older_siblings + 
  a241*motivation_behmgmt + a242*motivation_freeing + 
  a243*motivation_learning + a244*motivation_emoreg

screentime_weekly ~ z25*1 + d15*age + e15*gender + 
  f15L*ses.L + f15Q*ses.Q + f15C*ses.C + f15_4*ses.4 + f15_5*ses.5 +
  g15*childcare_halfdays + h15*num_older_siblings + 
  a251*motivation_behmgmt + a252*motivation_freeing + 
  a253*motivation_learning + a254*motivation_emoreg

# level 1 mediators
motivation_behmgmt ~ z11*1 + a11*age + i11*gender + 
  j11L*ses.L + j11Q*ses.Q + j11C*ses.C + j11_4*ses.4 + j11_5*ses.5 +
  k11*childcare_halfdays + l11*num_older_siblings

motivation_freeing ~ z12*1 + a12*age + i12*gender + 
  j12L*ses.L + j12Q*ses.Q + j12C*ses.C + j12_4*ses.4 + j12_5*ses.5 +
  k12*childcare_halfdays + l12*num_older_siblings

motivation_learning ~ z13*1 + a13*age + i13*gender + 
  j13L*ses.L + j13Q*ses.Q + j13C*ses.C + j13_4*ses.4 + j13_5*ses.5 +
  k13*childcare_halfdays + l13*num_older_siblings

motivation_emoreg ~ z14*1 + a14*age + i14*gender + 
  j14L*ses.L + j14Q*ses.Q + j14C*ses.C + j14_4*ses.4 + j14_5*ses.5 +
  k14*childcare_halfdays + l14*num_older_siblings

# Label threshold for supervision_accompanied
supervision_accompanied | t1*t1

# =============================================================================
# MODERATED MEDIATION ANALYSIS FOR CHILDCARE_HALFDAYS
# =============================================================================

# Scenario 1: X=0, M(X=0) - Natural reference state
motivation_behmgmt_00 := z11*1 + a11*0 + i11*0 + 
  j11L*ses6_L + j11Q*ses6_Q + j11C*ses6_C + j11_4*ses6_4 + j11_5*ses6_5 + 
  k11*0 + l11*0
motivation_freeing_00 := z12*1 + a12*0 + i12*0 + 
  j12L*ses6_L + j12Q*ses6_Q + j12C*ses6_C + j12_4*ses6_4 + j12_5*ses6_5 +
  k12*0 + l12*0
motivation_learning_00 := z13*1 + a13*0 + i13*0 + 
  j13L*ses6_L + j13Q*ses6_Q + j13C*ses6_C + j13_4*ses6_4 + j13_5*ses6_5 + 
  k13*0 + l13*0
motivation_emoreg_00 := z14*1 + a14*0 + i14*0 + 
  j14L*ses6_L + j14Q*ses6_Q + j14C*ses6_C + j14_4*ses6_4 + j14_5*ses6_5 + 
  k14*0 + l14*0

interaction_expanding_00 := z21*1 + d11*0 + e11*0 + 
  f11L*ses6_L + f11Q*ses6_Q + f11C*ses6_C + f11_4*ses6_4 + f11_5*ses6_5 + 
  g11*0 + h11*0 + 
  a211*motivation_behmgmt_00 + a212*motivation_freeing_00 + 
  a213*motivation_learning_00 + a214*motivation_emoreg_00

interaction_focusing_00 := z22*1 + d12*0 + e12*0 + 
  f12L*ses6_L + f12Q*ses6_Q + f12C*ses6_C + f12_4*ses6_4 + f12_5*ses6_5 + 
  g12*0 + h12*0 + 
  a221*motivation_behmgmt_00 + a222*motivation_freeing_00 + 
  a223*motivation_learning_00 + a224*motivation_emoreg_00

interaction_asking_00 := z23*1 + d13*0 + e13*0 + 
  f13L*ses6_L + f13Q*ses6_Q + f13C*ses6_C + f13_4*ses6_4 + f13_5*ses6_5 + 
  g13*0 + h13*0 + 
  a231*motivation_behmgmt_00 + a232*motivation_freeing_00 + 
  a233*motivation_learning_00 + a234*motivation_emoreg_00

screentime_weekly_00 := z25*1 + d15*0 + e15*0 + 
  f15L*ses6_L + f15Q*ses6_Q + f15C*ses6_C + f15_4*ses6_4 + f15_5*ses6_5 + 
  g15*0 + h15*0 + 
  a251*motivation_behmgmt_00 + a252*motivation_freeing_00 + 
  a253*motivation_learning_00 + a254*motivation_emoreg_00

supervision_linear_00 := t1 + d14*0 + e14*0 + 
  f14L*ses6_L + f14Q*ses6_Q + f14C*ses6_C + f14_4*ses6_4 + f14_5*ses6_5 + 
  g14*0 + h14*0 + 
  a241*motivation_behmgmt_00 + a242*motivation_freeing_00 + 
  a243*motivation_learning_00 + a244*motivation_emoreg_00

supervision_prob_00 := pnorm(supervision_linear_00)

outcome_00_childcare := z1*1 + c1*0 + c2*0 + 
  c3L*ses6_L + c3Q*ses6_Q + c3C*ses6_C + c3_4*ses6_4 + c3_5*ses6_5 + 
  c4*0 + c5*0 + 
  b11*motivation_behmgmt_00 + b12*motivation_freeing_00 + 
  b13*motivation_learning_00 + b14*motivation_emoreg_00 + 
  b21*interaction_expanding_00 + b22*interaction_focusing_00 + 
  b23*interaction_asking_00 + b24*supervision_prob_00 + 
  b25*screentime_weekly_00 +
  b26*interaction_expanding_00*screentime_weekly_00 + 
  b27*interaction_focusing_00*screentime_weekly_00 + 
  b28*interaction_asking_00*screentime_weekly_00 + 
  b29*supervision_prob_00*screentime_weekly_00 +
  b31*0*motivation_behmgmt_00 + b32*0*motivation_freeing_00 + 
  b33*0*motivation_learning_00 + b34*0*motivation_emoreg_00 + 
  b35*0*interaction_expanding_00 + b36*0*interaction_focusing_00 + 
  b37*0*interaction_asking_00 + b38*0*supervision_prob_00 + 
  b39*0*screentime_weekly_00

# Scenario 2: X=1, M(X=1) - Natural treatment state
motivation_behmgmt_11 := z11*1 + a11*0 + i11*0 + 
  j11L*ses6_L + j11Q*ses6_Q + j11C*ses6_C + j11_4*ses6_4 + j11_5*ses6_5 + 
  k11*1 + l11*0
motivation_freeing_11 := z12*1 + a12*0 + i12*0 + 
  j12L*ses6_L + j12Q*ses6_Q + j12C*ses6_C + j12_4*ses6_4 + j12_5*ses6_5 + 
  k12*1 + l12*0
motivation_learning_11 := z13*1 + a13*0 + i13*0 + 
  j13L*ses6_L + j13Q*ses6_Q + j13C*ses6_C + j13_4*ses6_4 + j13_5*ses6_5 +
  k13*1 + l13*0
motivation_emoreg_11 := z14*1 + a14*0 + i14*0 + 
  j14L*ses6_L + j14Q*ses6_Q + j14C*ses6_C + j14_4*ses6_4 + j14_5*ses6_5 + 
  k14*1 + l14*0

interaction_expanding_11 := z21*1 + d11*0 + e11*0 + 
  f11L*ses6_L + f11Q*ses6_Q + f11C*ses6_C + f11_4*ses6_4 + f11_5*ses6_5 + 
  g11*1 + h11*0 + 
  a211*motivation_behmgmt_11 + a212*motivation_freeing_11 + 
  a213*motivation_learning_11 + a214*motivation_emoreg_11

interaction_focusing_11 := z22*1 + d12*0 + e12*0 + 
  f12L*ses6_L + f12Q*ses6_Q + f12C*ses6_C + f12_4*ses6_4 + f12_5*ses6_5 + 
  g12*1 + h12*0 + 
  a221*motivation_behmgmt_11 + a222*motivation_freeing_11 + 
  a223*motivation_learning_11 + a224*motivation_emoreg_11

interaction_asking_11 := z23*1 + d13*0 + e13*0 + 
  f13L*ses6_L + f13Q*ses6_Q + f13C*ses6_C + f13_4*ses6_4 + f13_5*ses6_5 + 
  g13*1 + h13*0 + 
  a231*motivation_behmgmt_11 + a232*motivation_freeing_11 + 
  a233*motivation_learning_11 + a234*motivation_emoreg_11

screentime_weekly_11 := z25*1 + d15*0 + e15*0 + 
  f15L*ses6_L + f15Q*ses6_Q + f15C*ses6_C + f15_4*ses6_4 + f15_5*ses6_5 + 
  g15*1 + h15*0 + 
  a251*motivation_behmgmt_11 + a252*motivation_freeing_11 + 
  a253*motivation_learning_11 + a254*motivation_emoreg_11

supervision_linear_11 := t1 + d14*0 + e14*0 + 
  f14L*ses6_L + f14Q*ses6_Q + f14C*ses6_C + f14_4*ses6_4 + f14_5*ses6_5 + 
  g14*1 + h14*0 + 
  a241*motivation_behmgmt_11 + a242*motivation_freeing_11 + 
  a243*motivation_learning_11 + a244*motivation_emoreg_11

supervision_prob_11 := pnorm(supervision_linear_11)

outcome_11_childcare := z1*1 + c1*0 + c2*0 + 
  c3L*ses6_L + c3Q*ses6_Q + c3C*ses6_C + c3_4*ses6_4 + c3_5*ses6_5 + 
  c4*1 + c5*0 + 
  b11*motivation_behmgmt_11 + b12*motivation_freeing_11 + 
  b13*motivation_learning_11 + b14*motivation_emoreg_11 + 
  b21*interaction_expanding_11 + b22*interaction_focusing_11 + 
  b23*interaction_asking_11 + b24*supervision_prob_11 + 
  b25*screentime_weekly_11 +
  b26*interaction_expanding_11*screentime_weekly_11 + 
  b27*interaction_focusing_11*screentime_weekly_11 + 
  b28*interaction_asking_11*screentime_weekly_11 + 
  b29*supervision_prob_11*screentime_weekly_11 +
  b31*1*motivation_behmgmt_11 + b32*1*motivation_freeing_11 + 
  b33*1*motivation_learning_11 + b34*1*motivation_emoreg_11 + 
  b35*1*interaction_expanding_11 + b36*1*interaction_focusing_11 + 
  b37*1*interaction_asking_11 + b38*1*supervision_prob_11 + 
  b39*1*screentime_weekly_11

# Scenario 3: X=1, M(X=0) - Controlled direct effect
outcome_10_childcare := z1*1 + c1*0 + c2*0 + 
  c3L*ses6_L + c3Q*ses6_Q + c3C*ses6_C + c3_4*ses6_4 + c3_5*ses6_5 + 
  c4*1 + c5*0 + 
  b11*motivation_behmgmt_00 + b12*motivation_freeing_00 + 
  b13*motivation_learning_00 + b14*motivation_emoreg_00 + 
  b21*interaction_expanding_00 + b22*interaction_focusing_00 + 
  b23*interaction_asking_00 + b24*supervision_prob_00 + 
  b25*screentime_weekly_00 +
  b26*interaction_expanding_00*screentime_weekly_00 + 
  b27*interaction_focusing_00*screentime_weekly_00 + 
  b28*interaction_asking_00*screentime_weekly_00 + 
  b29*supervision_prob_00*screentime_weekly_00 +
  b31*1*motivation_behmgmt_00 + b32*1*motivation_freeing_00 + 
  b33*1*motivation_learning_00 + b34*1*motivation_emoreg_00 + 
  b35*1*interaction_expanding_00 + b36*1*interaction_focusing_00 + 
  b37*1*interaction_asking_00 + b38*1*supervision_prob_00 + 
  b39*1*screentime_weekly_00

# Scenario 4: X=0, M(X=1) - Pure mediation effect
outcome_01_childcare := z1*1 + c1*0 + c2*0 + 
  c3L*ses6_L + c3Q*ses6_Q + c3C*ses6_C + c3_4*ses6_4 + c3_5*ses6_5 + 
  c4*0 + c5*0 + 
  b11*motivation_behmgmt_11 + b12*motivation_freeing_11 + 
  b13*motivation_learning_11 + b14*motivation_emoreg_11 + 
  b21*interaction_expanding_11 + b22*interaction_focusing_11 + 
  b23*interaction_asking_11 + b24*supervision_prob_11 + 
  b25*screentime_weekly_11 +
  b26*interaction_expanding_11*screentime_weekly_11 + 
  b27*interaction_focusing_11*screentime_weekly_11 + 
  b28*interaction_asking_11*screentime_weekly_11 + 
  b29*supervision_prob_11*screentime_weekly_11 +
  b31*0*motivation_behmgmt_11 + b32*0*motivation_freeing_11 + 
  b33*0*motivation_learning_11 + b34*0*motivation_emoreg_11 + 
  b35*0*interaction_expanding_11 + b36*0*interaction_focusing_11 + 
  b37*0*interaction_asking_11 + b38*0*supervision_prob_11 + 
  b39*0*screentime_weekly_11

# =============================================================================
# CAUSAL MEDIATION PARAMETERS FOR CHILDCARE_HALFDAYS (WITH MODERATION)
# =============================================================================

# Pure Natural Direct Effect (PNDE)
PNDE_childcare := outcome_10_childcare - outcome_00_childcare

# Total Natural Direct Effect (TNDE) 
TNDE_childcare := outcome_11_childcare - outcome_01_childcare

# Pure Natural Indirect Effect (PNIE)
PNIE_childcare := outcome_01_childcare - outcome_00_childcare

# Total Natural Indirect Effect (TNIE)
TNIE_childcare := outcome_11_childcare - outcome_10_childcare

# Total Effect (TE)
TE_childcare := outcome_11_childcare - outcome_00_childcare

# Proportion Mediated (PM) - add small constant to avoid division by zero
PM_childcare := PNIE_childcare / (TE_childcare + 0.001)

# =============================================================================
# MODERATED MEDIATION ANALYSIS FOR NUM_OLDER_SIBLINGS
# =============================================================================

# Scenario 1: X=0, M(X=0) for siblings (num_older_siblings = 0)
motivation_behmgmt_00_sib := z11*1 + a11*0 + i11*0 +
  j11L*ses6_L + j11Q*ses6_Q + j11C*ses6_C + j11_4*ses6_4 + j11_5*ses6_5 +
  k11*0 + l11*0
motivation_freeing_00_sib := z12*1 + a12*0 + i12*0 +
  j12L*ses6_L + j12Q*ses6_Q + j12C*ses6_C + j12_4*ses6_4 + j12_5*ses6_5 +
  k12*0 + l12*0
motivation_learning_00_sib := z13*1 + a13*0 + i13*0 +
  j13L*ses6_L + j13Q*ses6_Q + j13C*ses6_C + j13_4*ses6_4 + j13_5*ses6_5 +
  k13*0 + l13*0
motivation_emoreg_00_sib := z14*1 + a14*0 + i14*0 +
  j14L*ses6_L + j14Q*ses6_Q + j14C*ses6_C + j14_4*ses6_4 + j14_5*ses6_5 +
  k14*0 + l14*0

interaction_expanding_00_sib := z21*1 + d11*0 + e11*0 + 
  f11L*ses6_L + f11Q*ses6_Q + f11C*ses6_C + f11_4*ses6_4 + f11_5*ses6_5 + 
  g11*0 + h11*0 + 
  a211*motivation_behmgmt_00_sib + a212*motivation_freeing_00_sib + 
  a213*motivation_learning_00_sib + a214*motivation_emoreg_00_sib

interaction_focusing_00_sib := z22*1 + d12*0 + e12*0 + 
  f12L*ses6_L + f12Q*ses6_Q + f12C*ses6_C + f12_4*ses6_4 + f12_5*ses6_5 + 
  g12*0 + h12*0 + 
  a221*motivation_behmgmt_00_sib + a222*motivation_freeing_00_sib + 
  a223*motivation_learning_00_sib + a224*motivation_emoreg_00_sib

interaction_asking_00_sib := z23*1 + d13*0 + e13*0 + 
  f13L*ses6_L + f13Q*ses6_Q + f13C*ses6_C + f13_4*ses6_4 + f13_5*ses6_5 + 
  g13*0 + h13*0 + 
  a231*motivation_behmgmt_00_sib + a232*motivation_freeing_00_sib + 
  a233*motivation_learning_00_sib + a234*motivation_emoreg_00_sib

screentime_weekly_00_sib := z25*1 + d15*0 + e15*0 + 
  f15L*ses6_L + f15Q*ses6_Q + f15C*ses6_C + f15_4*ses6_4 + f15_5*ses6_5 + 
  g15*0 + h15*0 + 
  a251*motivation_behmgmt_00_sib + a252*motivation_freeing_00_sib + 
  a253*motivation_learning_00_sib + a254*motivation_emoreg_00_sib

supervision_linear_00_sib := t1 + d14*0 + e14*0 + 
  f14L*ses6_L + f14Q*ses6_Q + f14C*ses6_C + f14_4*ses6_4 + f14_5*ses6_5 + 
  g14*0 + h14*0 + 
  a241*motivation_behmgmt_00_sib + a242*motivation_freeing_00_sib + 
  a243*motivation_learning_00_sib + a244*motivation_emoreg_00_sib

supervision_prob_00_sib := pnorm(supervision_linear_00_sib)

outcome_00_siblings := z1*1 + c1*0 + c2*0 + 
  c3L*ses6_L + c3Q*ses6_Q + c3C*ses6_C + c3_4*ses6_4 + c3_5*ses6_5 + 
  c4*0 + c5*0 + 
  b11*motivation_behmgmt_00_sib + b12*motivation_freeing_00_sib + 
  b13*motivation_learning_00_sib + b14*motivation_emoreg_00_sib + 
  b21*interaction_expanding_00_sib + b22*interaction_focusing_00_sib + 
  b23*interaction_asking_00_sib + b24*supervision_prob_00_sib + 
  b25*screentime_weekly_00_sib +
  b26*interaction_expanding_00_sib*screentime_weekly_00_sib + 
  b27*interaction_focusing_00_sib*screentime_weekly_00_sib + 
  b28*interaction_asking_00_sib*screentime_weekly_00_sib + 
  b29*supervision_prob_00_sib*screentime_weekly_00_sib +
  b41*0*motivation_behmgmt_00_sib + b42*0*motivation_freeing_00_sib + 
  b43*0*motivation_learning_00_sib + b44*0*motivation_emoreg_00_sib + 
  b45*0*interaction_expanding_00_sib + b46*0*interaction_focusing_00_sib + 
  b47*0*interaction_asking_00_sib + b48*0*supervision_prob_00_sib + 
  b49*0*screentime_weekly_00_sib

# Scenario 2: X=1, M(X=1) for siblings (num_older_siblings = 1)
motivation_behmgmt_11_sib := z11*1 + a11*0 + i11*0 +
  j11L*ses6_L + j11Q*ses6_Q + j11C*ses6_C + j11_4*ses6_4 + j11_5*ses6_5 +
  k11*0 + l11*1
motivation_freeing_11_sib := z12*1 + a12*0 + i12*0 +
  j12L*ses6_L + j12Q*ses6_Q + j12C*ses6_C + j12_4*ses6_4 + j12_5*ses6_5 +
  k12*0 + l12*1
motivation_learning_11_sib := z13*1 + a13*0 + i13*0 +
  j13L*ses6_L + j13Q*ses6_Q + j13C*ses6_C + j13_4*ses6_4 + j13_5*ses6_5 +
  k13*0 + l13*1
motivation_emoreg_11_sib := z14*1 + a14*0 + i14*0 +
  j14L*ses6_L + j14Q*ses6_Q + j14C*ses6_C + j14_4*ses6_4 + j14_5*ses6_5 +
  k14*0 + l14*1

interaction_expanding_11_sib := z21*1 + d11*0 + e11*0 + 
  f11L*ses6_L + f11Q*ses6_Q + f11C*ses6_C + f11_4*ses6_4 + f11_5*ses6_5 + 
  g11*0 + h11*1 + 
  a211*motivation_behmgmt_11_sib + a212*motivation_freeing_11_sib + 
  a213*motivation_learning_11_sib + a214*motivation_emoreg_11_sib

interaction_focusing_11_sib := z22*1 + d12*0 + e12*0 + 
  f12L*ses6_L + f12Q*ses6_Q + f12C*ses6_C + f12_4*ses6_4 + f12_5*ses6_5 + 
  g12*0 + h12*1 + 
  a221*motivation_behmgmt_11_sib + a222*motivation_freeing_11_sib + 
  a223*motivation_learning_11_sib + a224*motivation_emoreg_11_sib

interaction_asking_11_sib := z23*1 + d13*0 + e13*0 + 
  f13L*ses6_L + f13Q*ses6_Q + f13C*ses6_C + f13_4*ses6_4 + f13_5*ses6_5 + 
  g13*0 + h13*1 + 
  a231*motivation_behmgmt_11_sib + a232*motivation_freeing_11_sib + 
  a233*motivation_learning_11_sib + a234*motivation_emoreg_11_sib

screentime_weekly_11_sib := z25*1 + d15*0 + e15*0 + 
  f15L*ses6_L + f15Q*ses6_Q + f15C*ses6_C + f15_4*ses6_4 + f15_5*ses6_5 + 
  g15*0 + h15*1 + 
  a251*motivation_behmgmt_11_sib + a252*motivation_freeing_11_sib + 
  a253*motivation_learning_11_sib + a254*motivation_emoreg_11_sib

supervision_linear_11_sib := t1 + d14*0 + e14*0 + 
  f14L*ses6_L + f14Q*ses6_Q + f14C*ses6_C + f14_4*ses6_4 + f14_5*ses6_5 + 
  g14*0 + h14*1 + 
  a241*motivation_behmgmt_11_sib + a242*motivation_freeing_11_sib + 
  a243*motivation_learning_11_sib + a244*motivation_emoreg_11_sib

supervision_prob_11_sib := pnorm(supervision_linear_11_sib)

outcome_11_siblings := z1*1 + c1*0 + c2*0 + 
  c3L*ses6_L + c3Q*ses6_Q + c3C*ses6_C + c3_4*ses6_4 + c3_5*ses6_5 + 
  c4*0 + c5*1 + 
  b11*motivation_behmgmt_11_sib + b12*motivation_freeing_11_sib + 
  b13*motivation_learning_11_sib + b14*motivation_emoreg_11_sib + 
  b21*interaction_expanding_11_sib + b22*interaction_focusing_11_sib +
  b23*interaction_asking_11_sib + b24*supervision_prob_11_sib +
  b25*screentime_weekly_11_sib +
  b26*interaction_expanding_11_sib*screentime_weekly_11_sib +
  b27*interaction_focusing_11_sib*screentime_weekly_11_sib +
  b28*interaction_asking_11_sib*screentime_weekly_11_sib +
  b29*supervision_prob_11_sib*screentime_weekly_11_sib +
  b41*1*motivation_behmgmt_11_sib + b42*1*motivation_freeing_11_sib + 
  b43*1*motivation_learning_11_sib + b44*1*motivation_emoreg_11_sib + 
  b45*1*interaction_expanding_11_sib + b46*1*interaction_focusing_11_sib + 
  b47*1*interaction_asking_11_sib + b48*1*supervision_prob_11_sib + 
  b49*1*screentime_weekly_11_sib
  
# Scenario 3: X=1, M(X=0) for siblings (num_older_siblings = 0)
outcome_10_siblings := z1*1 + c1*0 + c2*0 + 
  c3L*ses6_L + c3Q*ses6_Q + c3C*ses6_C + c3_4*ses6_4 + c3_5*ses6_5 + 
  c4*0 + c5*1 + 
  b11*motivation_behmgmt_00_sib + b12*motivation_freeing_00_sib + 
  b13*motivation_learning_00_sib + b14*motivation_emoreg_00_sib + 
  b21*interaction_expanding_00_sib + b22*interaction_focusing_00_sib + 
  b23*interaction_asking_00_sib + b24*supervision_prob_00_sib + 
  b25*screentime_weekly_00_sib +
  b26*interaction_expanding_00_sib*screentime_weekly_00_sib + 
  b27*interaction_focusing_00_sib*screentime_weekly_00_sib + 
  b28*interaction_asking_00_sib*screentime_weekly_00_sib + 
  b29*supervision_prob_00_sib*screentime_weekly_00_sib +
  b41*1*motivation_behmgmt_00_sib + b42*1*motivation_freeing_00_sib + 
  b43*1*motivation_learning_00_sib + b44*1*motivation_emoreg_00_sib + 
  b45*1*interaction_expanding_00_sib + b46*1*interaction_focusing_00_sib + 
  b47*1*interaction_asking_00_sib + b48*1*supervision_prob_00_sib + 
  b49*1*screentime_weekly_00_sib

# Scenario 4: X=0, M(X=1) - Pure mediation effect
outcome_01_siblings := z1*1 + c1*0 + c2*0 + 
  c3L*ses6_L + c3Q*ses6_Q + c3C*ses6_C + c3_4*ses6_4 + c3_5*ses6_5 + 
  c4*0 + c5*0 + 
  b11*motivation_behmgmt_11_sib + b12*motivation_freeing_11_sib + 
  b13*motivation_learning_11_sib + b14*motivation_emoreg_11_sib + 
  b21*interaction_expanding_11_sib + b22*interaction_focusing_11_sib + 
  b23*interaction_asking_11_sib + b24*supervision_prob_11_sib + 
  b25*screentime_weekly_11_sib +
  b26*interaction_expanding_11_sib*screentime_weekly_11_sib + 
  b27*interaction_focusing_11_sib*screentime_weekly_11_sib + 
  b28*interaction_asking_11_sib*screentime_weekly_11_sib + 
  b29*supervision_prob_11_sib*screentime_weekly_11_sib +
  b41*0*motivation_behmgmt_11_sib + b42*0*motivation_freeing_11_sib + 
  b43*0*motivation_learning_11_sib + b44*0*motivation_emoreg_11_sib + 
  b45*0*interaction_expanding_11_sib + b46*0*interaction_focusing_11_sib + 
  b47*0*interaction_asking_11_sib + b48*0*supervision_prob_11_sib + 
  b49*0*screentime_weekly_11_sib
  
# =============================================================================
# CAUSAL MEDIATION PARAMETERS FOR NUM_OLDER_SIBLINGS (WITH MODERATION)
# =============================================================================

# Pure Natural Direct Effect (PNDE)
PNDE_siblings := outcome_10_siblings - outcome_00_siblings

# Total Natural Direct Effect (TNDE) 
TNDE_siblings := outcome_11_siblings - outcome_01_siblings

# Pure Natural Indirect Effect (PNIE)
PNIE_siblings := outcome_01_siblings - outcome_00_siblings

# Total Natural Indirect Effect (TNIE)
TNIE_siblings := outcome_11_siblings - outcome_10_siblings

# Total Effect (TE)
TE_siblings := outcome_11_siblings - outcome_00_siblings

# Proportion Mediated (PM) - add small constant to avoid division by zero
PM_siblings := PNIE_siblings / (TE_siblings + 0.001)
'
