/* Generated from genequal.scm by the CHICKEN compiler
   http://www.call-cc.org
   2013-05-23 11:50
   Version 4.8.2 (rev eb1a63e)
   windows-cygwin-x86 [ manyargs dload ptables ]
   compiled 2013-02-04 on LNGNYCL-FDB00M1 (CYGWIN_NT-5.1)
   command line: genequal.scm -output-file genequal.c -emit-all-import-libraries
   used units: library eval chicken_2dsyntax srfi_2d4
*/

#include "chicken.h"

static C_PTABLE_ENTRY *create_ptable(void);
C_noret_decl(C_library_toplevel)
C_externimport void C_ccall C_library_toplevel(C_word c,C_word d,C_word k) C_noret;
C_noret_decl(C_eval_toplevel)
C_externimport void C_ccall C_eval_toplevel(C_word c,C_word d,C_word k) C_noret;
C_noret_decl(C_chicken_2dsyntax_toplevel)
C_externimport void C_ccall C_chicken_2dsyntax_toplevel(C_word c,C_word d,C_word k) C_noret;
C_noret_decl(C_srfi_2d4_toplevel)
C_externimport void C_ccall C_srfi_2d4_toplevel(C_word c,C_word d,C_word k) C_noret;

static C_TLS C_word lf[26];
static double C_possibly_force_alignment;
static C_char C_TLS li0[] C_aligned={C_lihdr(0,0,15),40,108,111,111,112,32,99,108,105,115,116,49,56,57,41,0};
static C_char C_TLS li1[] C_aligned={C_lihdr(0,0,58),40,103,101,110,101,113,117,97,108,35,103,101,110,101,114,97,108,105,122,101,100,45,101,113,117,97,108,63,32,120,49,56,53,32,121,49,56,54,32,46,32,105,110,105,116,105,97,108,45,99,108,105,115,116,49,56,55,41,0,0,0,0,0,0};
static C_char C_TLS li2[] C_aligned={C_lihdr(0,0,26),40,102,95,54,57,51,32,120,50,48,56,32,121,50,48,57,32,99,108,105,115,116,50,49,48,41,0,0,0,0,0,0};
static C_char C_TLS li3[] C_aligned={C_lihdr(0,0,57),40,103,101,110,101,113,117,97,108,35,109,97,107,101,45,97,116,111,109,105,99,45,99,111,109,112,97,114,97,116,111,114,32,116,121,112,101,63,50,48,54,32,101,113,117,105,118,97,108,101,110,116,63,50,48,55,41,0,0,0,0,0,0,0};
static C_char C_TLS li4[] C_aligned={C_lihdr(0,0,17),40,102,95,55,49,56,32,120,50,49,54,32,121,50,49,55,41,0,0,0,0,0,0,0};
static C_char C_TLS li5[] C_aligned={C_lihdr(0,0,54),40,103,101,110,101,113,117,97,108,35,109,97,107,101,45,115,112,101,99,105,102,105,99,45,101,113,117,97,108,105,116,121,32,46,32,99,111,109,112,97,114,97,116,111,114,45,108,105,115,116,50,49,53,41,0,0};
static C_char C_TLS li6[] C_aligned={C_lihdr(0,0,16),40,108,111,111,112,32,120,50,50,52,32,121,50,50,53,41};
static C_char C_TLS li7[] C_aligned={C_lihdr(0,0,45),40,103,101,110,101,113,117,97,108,35,108,105,115,116,45,99,111,109,112,97,114,97,116,111,114,32,120,50,50,48,32,121,50,50,49,32,99,108,105,115,116,50,50,50,41,0,0,0};
static C_char C_TLS li8[] C_aligned={C_lihdr(0,0,16),40,100,111,108,111,111,112,50,53,50,32,105,50,53,52,41};
static C_char C_TLS li9[] C_aligned={C_lihdr(0,0,17),40,102,95,56,49,57,32,114,101,116,117,114,110,50,52,57,41,0,0,0,0,0,0,0};
static C_char C_TLS li10[] C_aligned={C_lihdr(0,0,47),40,103,101,110,101,113,117,97,108,35,118,101,99,116,111,114,45,99,111,109,112,97,114,97,116,111,114,32,120,50,51,50,32,121,50,51,51,32,99,108,105,115,116,50,51,52,41,0};
static C_char C_TLS li11[] C_aligned={C_lihdr(0,0,16),40,100,111,108,111,111,112,50,55,56,32,105,50,56,48,41};
static C_char C_TLS li12[] C_aligned={C_lihdr(0,0,17),40,102,95,56,57,55,32,114,101,116,117,114,110,50,55,53,41,0,0,0,0,0,0,0};
static C_char C_TLS li13[] C_aligned={C_lihdr(0,0,51),40,103,101,110,101,113,117,97,108,35,98,121,116,101,118,101,99,116,111,114,45,99,111,109,112,97,114,97,116,111,114,32,120,50,53,56,32,121,50,53,57,32,99,108,105,115,116,50,54,48,41,0,0,0,0,0};
static C_char C_TLS li14[] C_aligned={C_lihdr(0,0,10),40,116,111,112,108,101,118,101,108,41,0,0,0,0,0,0};


C_noret_decl(f_845)
static void C_ccall f_845(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_970)
static void C_ccall f_970(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_827)
static void C_fcall f_827(C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_701)
static void C_fcall f_701(C_word t0,C_word t1) C_noret;
C_noret_decl(f_836)
static void C_ccall f_836(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_491)
static void C_ccall f_491(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_710)
static void C_ccall f_710(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_713)
static void C_ccall f_713(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_715)
static void C_ccall f_715(C_word c,C_word t0,C_word t1,...) C_noret;
C_noret_decl(f_715)
static void C_ccall f_715r(C_word t0,C_word t1,C_word t3) C_noret;
C_noret_decl(f_718)
static void C_ccall f_718(C_word c,C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_886)
static void C_fcall f_886(C_word t0,C_word t1) C_noret;
C_noret_decl(f_693)
static void C_ccall f_693(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4) C_noret;
C_noret_decl(f_935)
static void C_ccall f_935(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_932)
static void C_ccall f_932(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_690)
static void C_ccall f_690(C_word c,C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_897)
static void C_ccall f_897(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_900)
static void C_ccall f_900(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_905)
static void C_fcall f_905(C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_611)
static void C_fcall f_611(C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_914)
static void C_ccall f_914(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_799)
static void C_ccall f_799(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4) C_noret;
C_noret_decl(f_944)
static void C_ccall f_944(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_941)
static void C_ccall f_941(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_953)
static void C_ccall f_953(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_950)
static void C_ccall f_950(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_957)
static void C_ccall f_957(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(C_toplevel)
C_externexport void C_ccall C_toplevel(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_733)
static void C_fcall f_733(C_word t0,C_word t1,C_word t2,C_word t3) C_noret;
C_noret_decl(f_623)
static void C_ccall f_623(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_661)
static void C_ccall f_661(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_487)
static void C_ccall f_487(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_489)
static void C_ccall f_489(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_599)
static void C_ccall f_599(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,...) C_noret;
C_noret_decl(f_599)
static void C_ccall f_599r(C_word t0,C_word t1,C_word t2,C_word t3,C_word t5) C_noret;
C_noret_decl(f_808)
static void C_fcall f_808(C_word t0,C_word t1) C_noret;
C_noret_decl(f_485)
static void C_ccall f_485(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_655)
static void C_ccall f_655(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_657)
static void C_ccall f_657(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_659)
static void C_ccall f_659(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_725)
static void C_ccall f_725(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_727)
static void C_ccall f_727(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4) C_noret;
C_noret_decl(f_965)
static void C_ccall f_965(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_967)
static void C_ccall f_967(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_877)
static void C_ccall f_877(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4) C_noret;
C_noret_decl(f_961)
static void C_ccall f_961(C_word c,C_word t0,C_word t1) C_noret;
C_noret_decl(f_819)
static void C_ccall f_819(C_word c,C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_770)
static void C_ccall f_770(C_word c,C_word t0,C_word t1) C_noret;

C_noret_decl(trf_827)
static void C_fcall trf_827(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_827(void *dummy){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
f_827(t0,t1,t2);}

C_noret_decl(trf_701)
static void C_fcall trf_701(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_701(void *dummy){
C_word t1=C_pick(0);
C_word t0=C_pick(1);
C_adjust_stack(-2);
f_701(t0,t1);}

C_noret_decl(trf_886)
static void C_fcall trf_886(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_886(void *dummy){
C_word t1=C_pick(0);
C_word t0=C_pick(1);
C_adjust_stack(-2);
f_886(t0,t1);}

C_noret_decl(trf_905)
static void C_fcall trf_905(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_905(void *dummy){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
f_905(t0,t1,t2);}

C_noret_decl(trf_611)
static void C_fcall trf_611(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_611(void *dummy){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
f_611(t0,t1,t2);}

C_noret_decl(trf_733)
static void C_fcall trf_733(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_733(void *dummy){
C_word t3=C_pick(0);
C_word t2=C_pick(1);
C_word t1=C_pick(2);
C_word t0=C_pick(3);
C_adjust_stack(-4);
f_733(t0,t1,t2,t3);}

C_noret_decl(trf_808)
static void C_fcall trf_808(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall trf_808(void *dummy){
C_word t1=C_pick(0);
C_word t0=C_pick(1);
C_adjust_stack(-2);
f_808(t0,t1);}

C_noret_decl(tr3)
static void C_fcall tr3(C_proc3 k) C_regparm C_noret;
C_regparm static void C_fcall tr3(C_proc3 k){
C_word t2=C_pick(0);
C_word t1=C_pick(1);
C_word t0=C_pick(2);
C_adjust_stack(-3);
(k)(3,t0,t1,t2);}

C_noret_decl(tr5)
static void C_fcall tr5(C_proc5 k) C_regparm C_noret;
C_regparm static void C_fcall tr5(C_proc5 k){
C_word t4=C_pick(0);
C_word t3=C_pick(1);
C_word t2=C_pick(2);
C_word t1=C_pick(3);
C_word t0=C_pick(4);
C_adjust_stack(-5);
(k)(5,t0,t1,t2,t3,t4);}

C_noret_decl(tr4)
static void C_fcall tr4(C_proc4 k) C_regparm C_noret;
C_regparm static void C_fcall tr4(C_proc4 k){
C_word t3=C_pick(0);
C_word t2=C_pick(1);
C_word t1=C_pick(2);
C_word t0=C_pick(3);
C_adjust_stack(-4);
(k)(4,t0,t1,t2,t3);}

C_noret_decl(tr2)
static void C_fcall tr2(C_proc2 k) C_regparm C_noret;
C_regparm static void C_fcall tr2(C_proc2 k){
C_word t1=C_pick(0);
C_word t0=C_pick(1);
C_adjust_stack(-2);
(k)(2,t0,t1);}

C_noret_decl(tr4r)
static void C_fcall tr4r(C_proc4 k) C_regparm C_noret;
C_regparm static void C_fcall tr4r(C_proc4 k){
int n;
C_word *a,t4;
C_word t3=C_pick(0);
C_word t2=C_pick(1);
C_word t1=C_pick(2);
C_word t0=C_pick(3);
C_adjust_stack(-4);
n=C_rest_count(0);
a=C_alloc(n*3);
t4=C_restore_rest(a,n);
(k)(t0,t1,t2,t3,t4);}

C_noret_decl(tr2r)
static void C_fcall tr2r(C_proc2 k) C_regparm C_noret;
C_regparm static void C_fcall tr2r(C_proc2 k){
int n;
C_word *a,t2;
C_word t1=C_pick(0);
C_word t0=C_pick(1);
C_adjust_stack(-2);
n=C_rest_count(0);
a=C_alloc(n*3);
t2=C_restore_rest(a,n);
(k)(t0,t1,t2);}

/* k843 in doloop252 */
static void C_ccall f_845(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
if(C_truep(t1)){
t2=((C_word*)t0)[2];
f_836(2,t2,C_SCHEME_UNDEFINED);}
else{
C_trace("genequal-impl.scm:64: return");
t2=((C_word*)t0)[3];
((C_proc3)C_fast_retrieve_proc(t2))(3,t2,((C_word*)t0)[2],C_SCHEME_FALSE);}}

/* k968 in k963 in k959 in k955 in k723 in k490 in k488 in k486 in k484 */
static void C_ccall f_970(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
t2=t1;
((C_proc2)C_fast_retrieve_proc(t2))(2,t2,((C_word*)t0)[2]);}

/* doloop252 */
static void C_fcall f_827(C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word ab[9],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_827,NULL,3,t0,t1,t2);}
if(C_truep(C_i_greater_or_equalp(t2,((C_word*)t0)[2]))){
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_SCHEME_TRUE);}
else{
t3=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_836,a[2]=t2,a[3]=((C_word*)t0)[3],a[4]=t1,tmp=(C_word)a,a+=5,tmp);
t4=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_845,a[2]=t3,a[3]=((C_word*)t0)[4],tmp=(C_word)a,a+=4,tmp);
t5=C_i_vector_ref(((C_word*)t0)[5],t2);
t6=C_i_vector_ref(((C_word*)t0)[6],t2);
C_apply(6,0,t4,*((C_word*)lf[2]+1),t5,t6,((C_word*)t0)[7]);}}

/* k699 in k712 */
static void C_fcall f_701(C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
if(C_truep(t1)){
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,lf[7]);}
else{
C_trace("genequal-impl.scm:31: equivalent?");
t2=((C_word*)t0)[3];
((C_proc4)C_fast_retrieve_proc(t2))(4,t2,((C_word*)t0)[2],((C_word*)t0)[4],((C_word*)t0)[5]);}}

/* k835 in doloop252 */
static void C_ccall f_836(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[4],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_836,2,t0,t1);}
t2=C_a_i_plus(&a,2,((C_word*)t0)[2],C_fix(1));
t3=((C_word*)((C_word*)t0)[3])[1];
f_827(t3,((C_word*)t0)[4],t2);}

/* k490 in k488 in k486 in k484 */
static void C_ccall f_491(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word ab[12],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_491,2,t0,t1);}
t2=C_mutate2(&lf[0] /* (set! genequal#bytevector? ...) */,C_fast_retrieve(lf[1]));
t3=C_mutate2((C_word*)lf[2]+1 /* (set! genequal#generalized-equal? ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_599,a[2]=((C_word)li1),tmp=(C_word)a,a+=3,tmp));
t4=C_mutate2((C_word*)lf[10]+1 /* (set! genequal#make-atomic-comparator ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_690,a[2]=((C_word)li3),tmp=(C_word)a,a+=3,tmp));
t5=C_mutate2((C_word*)lf[11]+1 /* (set! genequal#make-specific-equality ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_715,a[2]=((C_word)li5),tmp=(C_word)a,a+=3,tmp));
t6=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_725,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace("genequal-impl.scm:39: make-atomic-comparator");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[10]+1)))(4,*((C_word*)lf[10]+1),t6,*((C_word*)lf[24]+1),*((C_word*)lf[25]+1));}

/* k709 in k712 */
static void C_ccall f_710(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
t2=((C_word*)t0)[2];
f_701(t2,C_i_not(t1));}

/* k712 */
static void C_ccall f_713(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word ab[9],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_713,2,t0,t1);}
t2=C_i_not(t1);
t3=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_701,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[4],a[5]=((C_word*)t0)[5],tmp=(C_word)a,a+=6,tmp);
if(C_truep(t2)){
t4=t3;
f_701(t4,t2);}
else{
t4=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_710,a[2]=t3,tmp=(C_word)a,a+=3,tmp);
C_trace("genequal-impl.scm:29: type?");
t5=((C_word*)t0)[6];
((C_proc3)C_fast_retrieve_proc(t5))(3,t5,t4,((C_word*)t0)[5]);}}

/* genequal#make-specific-equality in k490 in k488 in k486 in k484 */
static void C_ccall f_715(C_word c,C_word t0,C_word t1,...){
C_word tmp;
C_word t2;
va_list v;
C_word *a,c2=c;
C_save_rest(t1,c2,2);
C_check_for_interrupt;
if(!C_demand(c*C_SIZEOF_PAIR+4)){
C_save_and_reclaim((void*)tr2r,(void*)f_715r,2,t0,t1);}
else{
a=C_alloc((c-2)*3);
t2=C_restore_rest(a,C_rest_count(0));
f_715r(t0,t1,t2);}}

static void C_ccall f_715r(C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a=C_alloc(4);
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_718,a[2]=t2,a[3]=((C_word)li4),tmp=(C_word)a,a+=4,tmp));}

/* f_718 in genequal#make-specific-equality in k490 in k488 in k486 in k484 */
static void C_ccall f_718(C_word c,C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr4,(void*)f_718,4,t0,t1,t2,t3);}
C_apply(6,0,t1,*((C_word*)lf[2]+1),t2,t3,((C_word*)t0)[2]);}

/* k884 in k952 in genequal#bytevector-comparator in k723 in k490 in k488 in k486 in k484 */
static void C_fcall f_886(C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[5],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_886,NULL,2,t0,t1);}
if(C_truep(t1)){
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,lf[7]);}
else{
t2=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_941,a[2]=((C_word*)t0)[3],a[3]=((C_word*)t0)[4],a[4]=((C_word*)t0)[2],tmp=(C_word)a,a+=5,tmp);
C_trace("genequal-impl.scm:72: bytevector-length");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[15]+1)))(3,*((C_word*)lf[15]+1),t2,((C_word*)t0)[4]);}}

/* f_693 in genequal#make-atomic-comparator in k490 in k488 in k486 in k484 */
static void C_ccall f_693(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4){
C_word tmp;
C_word t5;
C_word t6;
C_word ab[7],*a=ab;
if(c!=5) C_bad_argc_2(c,5,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr5,(void*)f_693,5,t0,t1,t2,t3,t4);}
t5=(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_713,a[2]=t1,a[3]=((C_word*)t0)[2],a[4]=t2,a[5]=t3,a[6]=((C_word*)t0)[3],tmp=(C_word)a,a+=7,tmp);
C_trace("genequal-impl.scm:29: type?");
t6=((C_word*)t0)[3];
((C_proc3)C_fast_retrieve_proc(t6))(3,t6,t5,t2);}

/* k934 in k931 in doloop278 in k899 */
static void C_ccall f_935(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
if(C_truep(C_i_nequalp(((C_word*)t0)[2],t1))){
t2=((C_word*)t0)[3];
f_914(2,t2,C_SCHEME_UNDEFINED);}
else{
C_trace("genequal-impl.scm:77: return");
t2=((C_word*)t0)[4];
((C_proc3)C_fast_retrieve_proc(t2))(3,t2,((C_word*)t0)[3],C_SCHEME_FALSE);}}

/* k931 in doloop278 in k899 */
static void C_ccall f_932(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[5],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_932,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_935,a[2]=t1,a[3]=((C_word*)t0)[2],a[4]=((C_word*)t0)[3],tmp=(C_word)a,a+=5,tmp);
C_trace("genequal-impl.scm:76: bytevector-u8-ref");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[14]+1)))(4,*((C_word*)lf[14]+1),t2,((C_word*)t0)[4],((C_word*)t0)[5]);}

/* genequal#make-atomic-comparator in k490 in k488 in k486 in k484 */
static void C_ccall f_690(C_word c,C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word ab[5],*a=ab;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr4,(void*)f_690,4,t0,t1,t2,t3);}
t4=t1;
((C_proc2)(void*)(*((C_word*)t4+1)))(2,t4,(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_693,a[2]=t3,a[3]=t2,a[4]=((C_word)li2),tmp=(C_word)a,a+=5,tmp));}

/* f_897 in k943 in k940 in k884 in k952 in genequal#bytevector-comparator in k723 in k490 in k488 in k486 in k484 */
static void C_ccall f_897(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word ab[6],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_897,3,t0,t1,t2);}
t3=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_900,a[2]=t2,a[3]=((C_word*)t0)[2],a[4]=((C_word*)t0)[3],a[5]=t1,tmp=(C_word)a,a+=6,tmp);
C_trace("genequal-impl.scm:75: bytevector-length");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[15]+1)))(3,*((C_word*)lf[15]+1),t3,((C_word*)t0)[3]);}

/* k899 */
static void C_ccall f_900(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word ab[10],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_900,2,t0,t1);}
t2=C_SCHEME_UNDEFINED;
t3=(*a=C_VECTOR_TYPE|1,a[1]=t2,tmp=(C_word)a,a+=2,tmp);
t4=C_set_block_item(t3,0,(*a=C_CLOSURE_TYPE|7,a[1]=(C_word)f_905,a[2]=t1,a[3]=t3,a[4]=((C_word*)t0)[2],a[5]=((C_word*)t0)[3],a[6]=((C_word*)t0)[4],a[7]=((C_word)li11),tmp=(C_word)a,a+=8,tmp));
t5=((C_word*)t3)[1];
f_905(t5,((C_word*)t0)[5],C_fix(0));}

/* doloop278 in k899 */
static void C_fcall f_905(C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word ab[11],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_905,NULL,3,t0,t1,t2);}
if(C_truep(C_i_greater_or_equalp(t2,((C_word*)t0)[2]))){
t3=t1;
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,C_SCHEME_TRUE);}
else{
t3=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_914,a[2]=t2,a[3]=((C_word*)t0)[3],a[4]=t1,tmp=(C_word)a,a+=5,tmp);
t4=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_932,a[2]=t3,a[3]=((C_word*)t0)[4],a[4]=((C_word*)t0)[5],a[5]=t2,tmp=(C_word)a,a+=6,tmp);
C_trace("genequal-impl.scm:76: bytevector-u8-ref");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[14]+1)))(4,*((C_word*)lf[14]+1),t4,((C_word*)t0)[6],t2);}}

/* loop in genequal#generalized-equal? in k490 in k488 in k486 in k484 */
static void C_fcall f_611(C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word ab[6],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_611,NULL,3,t0,t1,t2);}
if(C_truep(C_i_nullp(t2))){
t3=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_655,a[2]=t1,a[3]=((C_word*)t0)[2],a[4]=((C_word*)t0)[3],a[5]=((C_word*)t0)[4],tmp=(C_word)a,a+=6,tmp);
C_trace("genequal-impl.scm:15: list-comparator");
((C_proc5)C_fast_retrieve_symbol_proc(lf[6]))(5,*((C_word*)lf[6]+1),t3,((C_word*)t0)[2],((C_word*)t0)[3],((C_word*)t0)[4]);}
else{
t3=C_i_car(t2);
t4=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_623,a[2]=t1,a[3]=t2,a[4]=((C_word*)t0)[5],tmp=(C_word)a,a+=5,tmp);
C_trace("genequal-impl.scm:9: g195");
t5=t3;
((C_proc5)C_fast_retrieve_proc(t5))(5,t5,t4,((C_word*)t0)[2],((C_word*)t0)[3],((C_word*)t0)[4]);}}

/* k913 in doloop278 in k899 */
static void C_ccall f_914(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[4],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_914,2,t0,t1);}
t2=C_a_i_plus(&a,2,((C_word*)t0)[2],C_fix(1));
t3=((C_word*)((C_word*)t0)[3])[1];
f_905(t3,((C_word*)t0)[4],t2);}

/* genequal#vector-comparator in k723 in k490 in k488 in k486 in k484 */
static void C_ccall f_799(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4){
C_word tmp;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word ab[6],*a=ab;
if(c!=5) C_bad_argc_2(c,5,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr5,(void*)f_799,5,t0,t1,t2,t3,t4);}
t5=C_i_vectorp(t2);
t6=C_i_not(t5);
t7=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_808,a[2]=t1,a[3]=t2,a[4]=t3,a[5]=t4,tmp=(C_word)a,a+=6,tmp);
if(C_truep(t6)){
t8=t7;
f_808(t8,t6);}
else{
t8=C_i_vectorp(t3);
t9=t7;
f_808(t9,C_i_not(t8));}}

/* k943 in k940 in k884 in k952 in genequal#bytevector-comparator in k723 in k490 in k488 in k486 in k484 */
static void C_ccall f_944(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[5],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_944,2,t0,t1);}
if(C_truep(C_i_nequalp(((C_word*)t0)[2],t1))){
t2=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_897,a[2]=((C_word*)t0)[3],a[3]=((C_word*)t0)[4],a[4]=((C_word)li12),tmp=(C_word)a,a+=5,tmp);
C_trace("genequal-impl.scm:75: call/cc");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[13]+1)))(3,*((C_word*)lf[13]+1),((C_word*)t0)[5],t2);}
else{
t2=((C_word*)t0)[5];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,C_SCHEME_FALSE);}}

/* k940 in k884 in k952 in genequal#bytevector-comparator in k723 in k490 in k488 in k486 in k484 */
static void C_ccall f_941(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[6],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_941,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_944,a[2]=t1,a[3]=((C_word*)t0)[2],a[4]=((C_word*)t0)[3],a[5]=((C_word*)t0)[4],tmp=(C_word)a,a+=6,tmp);
C_trace("genequal-impl.scm:72: bytevector-length");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[15]+1)))(3,*((C_word*)lf[15]+1),t2,((C_word*)t0)[2]);}

/* k952 in genequal#bytevector-comparator in k723 in k490 in k488 in k486 in k484 */
static void C_ccall f_953(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word ab[8],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_953,2,t0,t1);}
t2=C_i_not(t1);
t3=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_886,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[4],tmp=(C_word)a,a+=5,tmp);
if(C_truep(t2)){
t4=t3;
f_886(t4,t2);}
else{
t4=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_950,a[2]=t3,tmp=(C_word)a,a+=3,tmp);
C_trace("genequal-impl.scm:70: bytevector?");
((C_proc3)C_fast_retrieve_proc(lf[0]))(3,lf[0],t4,((C_word*)t0)[3]);}}

/* k949 in k952 in genequal#bytevector-comparator in k723 in k490 in k488 in k486 in k484 */
static void C_ccall f_950(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
t2=((C_word*)t0)[2];
f_886(t2,C_i_not(t1));}

/* k955 in k723 in k490 in k488 in k486 in k484 */
static void C_ccall f_957(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_957,2,t0,t1);}
t2=C_mutate2((C_word*)lf[5]+1 /* (set! genequal#string-comparator ...) */,t1);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_961,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace("genequal-impl.scm:81: make-atomic-comparator");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[10]+1)))(4,*((C_word*)lf[10]+1),t3,*((C_word*)lf[21]+1),*((C_word*)lf[22]+1));}

/* toplevel */
static C_TLS int toplevel_initialized=0;
C_main_entry_point
C_noret_decl(toplevel_trampoline)
static void C_fcall toplevel_trampoline(void *dummy) C_regparm C_noret;
C_regparm static void C_fcall toplevel_trampoline(void *dummy){
C_toplevel(2,C_SCHEME_UNDEFINED,C_restore);}

void C_ccall C_toplevel(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word *a;
if(toplevel_initialized) C_kontinue(t1,C_SCHEME_UNDEFINED);
else C_toplevel_entry(C_text("toplevel"));
C_check_nursery_minimum(3);
if(!C_demand(3)){
C_save(t1);
C_reclaim((void*)toplevel_trampoline,NULL);}
toplevel_initialized=1;
if(!C_demand_2(240)){
C_save(t1);
C_rereclaim2(240*sizeof(C_word), 1);
t1=C_restore;}
a=C_alloc(3);
C_initialize_lf(lf,26);
lf[1]=C_h_intern(&lf[1],9,"u8vector\077");
lf[2]=C_h_intern(&lf[2],27,"genequal#generalized-equal\077");
lf[3]=C_h_intern(&lf[3],30,"genequal#bytevector-comparator");
lf[4]=C_h_intern(&lf[4],26,"genequal#vector-comparator");
lf[5]=C_h_intern(&lf[5],26,"genequal#string-comparator");
lf[6]=C_h_intern(&lf[6],24,"genequal#list-comparator");
lf[7]=C_h_intern(&lf[7],4,"pass");
lf[8]=C_h_intern(&lf[8],5,"error");
lf[9]=C_decode_literal(C_heaptop,"\376B\000\0002generalized-equal\077: comparator returned bad result");
lf[10]=C_h_intern(&lf[10],31,"genequal#make-atomic-comparator");
lf[11]=C_h_intern(&lf[11],31,"genequal#make-specific-equality");
lf[12]=C_h_intern(&lf[12],27,"genequal#numeric-comparator");
lf[13]=C_h_intern(&lf[13],7,"call/cc");
lf[14]=C_h_intern(&lf[14],12,"u8vector-ref");
lf[15]=C_h_intern(&lf[15],15,"u8vector-length");
lf[16]=C_h_intern(&lf[16],29,"genequal#string-ci-comparator");
lf[17]=C_h_intern(&lf[17],27,"genequal#char-ci-comparator");
lf[18]=C_h_intern(&lf[18],25,"\003sysimplicit-exit-handler");
lf[19]=C_h_intern(&lf[19],5,"char\077");
lf[20]=C_h_intern(&lf[20],9,"char-ci=\077");
lf[21]=C_h_intern(&lf[21],7,"string\077");
lf[22]=C_h_intern(&lf[22],11,"string-ci=\077");
lf[23]=C_h_intern(&lf[23],8,"string=\077");
lf[24]=C_h_intern(&lf[24],7,"number\077");
lf[25]=C_h_intern(&lf[25],1,"=");
C_register_lf2(lf,26,create_ptable());
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_485,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
C_library_toplevel(2,C_SCHEME_UNDEFINED,t2);}

/* loop in genequal#list-comparator in k723 in k490 in k488 in k486 in k484 */
static void C_fcall f_733(C_word t0,C_word t1,C_word t2,C_word t3){
C_word tmp;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word ab[6],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_733,NULL,4,t0,t1,t2,t3);}
t4=C_i_nullp(t2);
t5=(C_truep(t4)?C_i_nullp(t3):C_SCHEME_FALSE);
if(C_truep(t5)){
t6=t1;
((C_proc2)(void*)(*((C_word*)t6+1)))(2,t6,C_SCHEME_TRUE);}
else{
if(C_truep(C_i_nullp(t2))){
t6=t1;
((C_proc2)(void*)(*((C_word*)t6+1)))(2,t6,C_SCHEME_FALSE);}
else{
if(C_truep(C_i_nullp(t3))){
t6=t1;
((C_proc2)(void*)(*((C_word*)t6+1)))(2,t6,C_SCHEME_FALSE);}
else{
if(C_truep(C_i_pairp(t2))){
if(C_truep(C_i_pairp(t3))){
t6=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_770,a[2]=t2,a[3]=t3,a[4]=((C_word*)t0)[2],a[5]=t1,tmp=(C_word)a,a+=6,tmp);
t7=C_i_car(t2);
t8=C_i_car(t3);
C_apply(6,0,t6,*((C_word*)lf[2]+1),t7,t8,((C_word*)t0)[3]);}
else{
t6=t1;
((C_proc2)(void*)(*((C_word*)t6+1)))(2,t6,lf[7]);}}
else{
t6=t1;
((C_proc2)(void*)(*((C_word*)t6+1)))(2,t6,lf[7]);}}}}}

/* k621 in loop in genequal#generalized-equal? in k490 in k488 in k486 in k484 */
static void C_ccall f_623(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word *a;
switch(t1){
case C_SCHEME_TRUE:
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,C_SCHEME_TRUE);
case C_SCHEME_FALSE:
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,C_SCHEME_FALSE);
default:
t2=C_eqp(t1,lf[7]);
if(C_truep(t2)){
t3=C_i_cdr(((C_word*)t0)[3]);
C_trace("genequal-impl.scm:12: loop");
t4=((C_word*)((C_word*)t0)[4])[1];
f_611(t4,((C_word*)t0)[2],t3);}
else{
t3=C_i_car(((C_word*)t0)[3]);
C_trace("genequal-impl.scm:13: error");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[8]+1)))(4,*((C_word*)lf[8]+1),((C_word*)t0)[2],lf[9],t3);}}}

/* k660 in k658 in k656 in k654 in loop in genequal#generalized-equal? in k490 in k488 in k486 in k484 */
static void C_ccall f_661(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word *a;
if(C_truep(C_booleanp(((C_word*)t0)[2]))){
t2=((C_word*)t0)[3];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,((C_word*)t0)[2]);}
else{
if(C_truep(C_booleanp(((C_word*)t0)[4]))){
t2=((C_word*)t0)[3];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,((C_word*)t0)[4]);}
else{
if(C_truep(C_booleanp(((C_word*)t0)[5]))){
t2=((C_word*)t0)[3];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,((C_word*)t0)[5]);}
else{
t2=C_booleanp(t1);
t3=((C_word*)t0)[3];
((C_proc2)(void*)(*((C_word*)t3+1)))(2,t3,(C_truep(t2)?t1:C_SCHEME_FALSE));}}}}

/* k486 in k484 */
static void C_ccall f_487(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_487,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_489,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_chicken_2dsyntax_toplevel(2,C_SCHEME_UNDEFINED,t2);}

/* k488 in k486 in k484 */
static void C_ccall f_489(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_489,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_491,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_srfi_2d4_toplevel(2,C_SCHEME_UNDEFINED,t2);}

/* genequal#generalized-equal? in k490 in k488 in k486 in k484 */
static void C_ccall f_599(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,...){
C_word tmp;
C_word t4;
va_list v;
C_word *a,c2=c;
C_save_rest(t3,c2,4);
if(c<4) C_bad_min_argc_2(c,4,t0);
C_check_for_interrupt;
if(!C_demand(c*C_SIZEOF_PAIR+9)){
C_save_and_reclaim((void*)tr4r,(void*)f_599r,4,t0,t1,t2,t3);}
else{
a=C_alloc((c-4)*3);
t4=C_restore_rest(a,C_rest_count(0));
f_599r(t0,t1,t2,t3,t4);}}

static void C_ccall f_599r(C_word t0,C_word t1,C_word t2,C_word t3,C_word t4){
C_word tmp;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word *a=C_alloc(9);
if(C_truep(C_i_eqvp(t2,t3))){
t5=t1;
((C_proc2)(void*)(*((C_word*)t5+1)))(2,t5,C_SCHEME_TRUE);}
else{
t5=C_SCHEME_UNDEFINED;
t6=(*a=C_VECTOR_TYPE|1,a[1]=t5,tmp=(C_word)a,a+=2,tmp);
t7=C_set_block_item(t6,0,(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_611,a[2]=t2,a[3]=t3,a[4]=t4,a[5]=t6,a[6]=((C_word)li0),tmp=(C_word)a,a+=7,tmp));
t8=((C_word*)t6)[1];
f_611(t8,t1,t4);}}

/* k806 in genequal#vector-comparator in k723 in k490 in k488 in k486 in k484 */
static void C_fcall f_808(C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word ab[6],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)trf_808,NULL,2,t0,t1);}
if(C_truep(t1)){
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,lf[7]);}
else{
t2=C_i_vector_length(((C_word*)t0)[3]);
t3=C_i_vector_length(((C_word*)t0)[4]);
if(C_truep(C_i_nequalp(t2,t3))){
t4=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_819,a[2]=((C_word*)t0)[3],a[3]=((C_word*)t0)[4],a[4]=((C_word*)t0)[5],a[5]=((C_word)li9),tmp=(C_word)a,a+=6,tmp);
C_trace("genequal-impl.scm:59: call/cc");
((C_proc3)C_fast_retrieve_proc(*((C_word*)lf[13]+1)))(3,*((C_word*)lf[13]+1),((C_word*)t0)[2],t4);}
else{
t4=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t4+1)))(2,t4,C_SCHEME_FALSE);}}}

/* k484 */
static void C_ccall f_485(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_485,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_487,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_eval_toplevel(2,C_SCHEME_UNDEFINED,t2);}

/* k654 in loop in genequal#generalized-equal? in k490 in k488 in k486 in k484 */
static void C_ccall f_655(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[7],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_655,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_657,a[2]=t1,a[3]=((C_word*)t0)[2],a[4]=((C_word*)t0)[3],a[5]=((C_word*)t0)[4],a[6]=((C_word*)t0)[5],tmp=(C_word)a,a+=7,tmp);
C_trace("genequal-impl.scm:16: string-comparator");
((C_proc5)C_fast_retrieve_symbol_proc(lf[5]))(5,*((C_word*)lf[5]+1),t2,((C_word*)t0)[3],((C_word*)t0)[4],((C_word*)t0)[5]);}

/* k656 in k654 in loop in genequal#generalized-equal? in k490 in k488 in k486 in k484 */
static void C_ccall f_657(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[8],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_657,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|7,a[1]=(C_word)f_659,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=t1,a[5]=((C_word*)t0)[4],a[6]=((C_word*)t0)[5],a[7]=((C_word*)t0)[6],tmp=(C_word)a,a+=8,tmp);
C_trace("genequal-impl.scm:17: vector-comparator");
((C_proc5)C_fast_retrieve_symbol_proc(lf[4]))(5,*((C_word*)lf[4]+1),t2,((C_word*)t0)[4],((C_word*)t0)[5],((C_word*)t0)[6]);}

/* k658 in k656 in k654 in loop in genequal#generalized-equal? in k490 in k488 in k486 in k484 */
static void C_ccall f_659(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word ab[6],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_659,2,t0,t1);}
t2=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_661,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[4],a[5]=t1,tmp=(C_word)a,a+=6,tmp);
C_trace("genequal-impl.scm:18: bytevector-comparator");
((C_proc5)C_fast_retrieve_symbol_proc(lf[3]))(5,*((C_word*)lf[3]+1),t2,((C_word*)t0)[5],((C_word*)t0)[6],((C_word*)t0)[7]);}

/* k723 in k490 in k488 in k486 in k484 */
static void C_ccall f_725(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word ab[12],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_725,2,t0,t1);}
t2=C_mutate2((C_word*)lf[12]+1 /* (set! genequal#numeric-comparator ...) */,t1);
t3=C_mutate2((C_word*)lf[6]+1 /* (set! genequal#list-comparator ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_727,a[2]=((C_word)li7),tmp=(C_word)a,a+=3,tmp));
t4=C_mutate2((C_word*)lf[4]+1 /* (set! genequal#vector-comparator ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_799,a[2]=((C_word)li10),tmp=(C_word)a,a+=3,tmp));
t5=C_mutate2((C_word*)lf[3]+1 /* (set! genequal#bytevector-comparator ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_877,a[2]=((C_word)li13),tmp=(C_word)a,a+=3,tmp));
t6=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_957,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace("genequal-impl.scm:79: make-atomic-comparator");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[10]+1)))(4,*((C_word*)lf[10]+1),t6,*((C_word*)lf[21]+1),*((C_word*)lf[23]+1));}

/* genequal#list-comparator in k723 in k490 in k488 in k486 in k484 */
static void C_ccall f_727(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4){
C_word tmp;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word ab[7],*a=ab;
if(c!=5) C_bad_argc_2(c,5,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr5,(void*)f_727,5,t0,t1,t2,t3,t4);}
t5=C_SCHEME_UNDEFINED;
t6=(*a=C_VECTOR_TYPE|1,a[1]=t5,tmp=(C_word)a,a+=2,tmp);
t7=C_set_block_item(t6,0,(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_733,a[2]=t6,a[3]=t4,a[4]=((C_word)li6),tmp=(C_word)a,a+=5,tmp));
t8=((C_word*)t6)[1];
f_733(t8,t1,t2,t3);}

/* k963 in k959 in k955 in k723 in k490 in k488 in k486 in k484 */
static void C_ccall f_965(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word ab[6],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_965,2,t0,t1);}
t2=C_mutate2((C_word*)lf[17]+1 /* (set! genequal#char-ci-comparator ...) */,t1);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_967,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t4=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_970,a[2]=t3,tmp=(C_word)a,a+=3,tmp);
C_trace("##sys#implicit-exit-handler");
((C_proc2)C_fast_retrieve_symbol_proc(lf[18]))(2,*((C_word*)lf[18]+1),t4);}

/* k966 in k963 in k959 in k955 in k723 in k490 in k488 in k486 in k484 */
static void C_ccall f_967(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word *a;
t2=((C_word*)t0)[2];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,C_SCHEME_UNDEFINED);}

/* genequal#bytevector-comparator in k723 in k490 in k488 in k486 in k484 */
static void C_ccall f_877(C_word c,C_word t0,C_word t1,C_word t2,C_word t3,C_word t4){
C_word tmp;
C_word t5;
C_word t6;
C_word ab[5],*a=ab;
if(c!=5) C_bad_argc_2(c,5,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr5,(void*)f_877,5,t0,t1,t2,t3,t4);}
t5=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_953,a[2]=t1,a[3]=t3,a[4]=t2,tmp=(C_word)a,a+=5,tmp);
C_trace("genequal-impl.scm:70: bytevector?");
((C_proc3)C_fast_retrieve_proc(lf[0]))(3,lf[0],t5,t2);}

/* k959 in k955 in k723 in k490 in k488 in k486 in k484 */
static void C_ccall f_961(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word ab[3],*a=ab;
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr2,(void*)f_961,2,t0,t1);}
t2=C_mutate2((C_word*)lf[16]+1 /* (set! genequal#string-ci-comparator ...) */,t1);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_965,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace("genequal-impl.scm:83: make-atomic-comparator");
((C_proc4)C_fast_retrieve_proc(*((C_word*)lf[10]+1)))(4,*((C_word*)lf[10]+1),t3,*((C_word*)lf[19]+1),*((C_word*)lf[20]+1));}

/* f_819 in k806 in genequal#vector-comparator in k723 in k490 in k488 in k486 in k484 */
static void C_ccall f_819(C_word c,C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word ab[11],*a=ab;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(!C_stack_probe(&a)){
C_save_and_reclaim((void*)tr3,(void*)f_819,3,t0,t1,t2);}
t3=C_i_vector_length(((C_word*)t0)[2]);
t4=C_SCHEME_UNDEFINED;
t5=(*a=C_VECTOR_TYPE|1,a[1]=t4,tmp=(C_word)a,a+=2,tmp);
t6=C_set_block_item(t5,0,(*a=C_CLOSURE_TYPE|8,a[1]=(C_word)f_827,a[2]=t3,a[3]=t5,a[4]=t2,a[5]=((C_word*)t0)[2],a[6]=((C_word*)t0)[3],a[7]=((C_word*)t0)[4],a[8]=((C_word)li8),tmp=(C_word)a,a+=9,tmp));
t7=((C_word*)t5)[1];
f_827(t7,t1,C_fix(0));}

/* k768 in loop in genequal#list-comparator in k723 in k490 in k488 in k486 in k484 */
static void C_ccall f_770(C_word c,C_word t0,C_word t1){
C_word tmp;
C_word t2;
C_word t3;
C_word t4;
C_word *a;
if(C_truep(t1)){
t2=C_i_cdr(((C_word*)t0)[2]);
t3=C_i_cdr(((C_word*)t0)[3]);
C_trace("genequal-impl.scm:49: loop");
t4=((C_word*)((C_word*)t0)[4])[1];
f_733(t4,((C_word*)t0)[5],t2,t3);}
else{
t2=((C_word*)t0)[5];
((C_proc2)(void*)(*((C_word*)t2+1)))(2,t2,C_SCHEME_FALSE);}}

#ifdef C_ENABLE_PTABLES
static C_PTABLE_ENTRY ptable[47] = {
{"f_845:genequal_2escm",(void*)f_845},
{"f_970:genequal_2escm",(void*)f_970},
{"f_827:genequal_2escm",(void*)f_827},
{"f_701:genequal_2escm",(void*)f_701},
{"f_836:genequal_2escm",(void*)f_836},
{"f_491:genequal_2escm",(void*)f_491},
{"f_710:genequal_2escm",(void*)f_710},
{"f_713:genequal_2escm",(void*)f_713},
{"f_715:genequal_2escm",(void*)f_715},
{"f_718:genequal_2escm",(void*)f_718},
{"f_886:genequal_2escm",(void*)f_886},
{"f_693:genequal_2escm",(void*)f_693},
{"f_935:genequal_2escm",(void*)f_935},
{"f_932:genequal_2escm",(void*)f_932},
{"f_690:genequal_2escm",(void*)f_690},
{"f_897:genequal_2escm",(void*)f_897},
{"f_900:genequal_2escm",(void*)f_900},
{"f_905:genequal_2escm",(void*)f_905},
{"f_611:genequal_2escm",(void*)f_611},
{"f_914:genequal_2escm",(void*)f_914},
{"f_799:genequal_2escm",(void*)f_799},
{"f_944:genequal_2escm",(void*)f_944},
{"f_941:genequal_2escm",(void*)f_941},
{"f_953:genequal_2escm",(void*)f_953},
{"f_950:genequal_2escm",(void*)f_950},
{"f_957:genequal_2escm",(void*)f_957},
{"toplevel:genequal_2escm",(void*)C_toplevel},
{"f_733:genequal_2escm",(void*)f_733},
{"f_623:genequal_2escm",(void*)f_623},
{"f_661:genequal_2escm",(void*)f_661},
{"f_487:genequal_2escm",(void*)f_487},
{"f_489:genequal_2escm",(void*)f_489},
{"f_599:genequal_2escm",(void*)f_599},
{"f_808:genequal_2escm",(void*)f_808},
{"f_485:genequal_2escm",(void*)f_485},
{"f_655:genequal_2escm",(void*)f_655},
{"f_657:genequal_2escm",(void*)f_657},
{"f_659:genequal_2escm",(void*)f_659},
{"f_725:genequal_2escm",(void*)f_725},
{"f_727:genequal_2escm",(void*)f_727},
{"f_965:genequal_2escm",(void*)f_965},
{"f_967:genequal_2escm",(void*)f_967},
{"f_877:genequal_2escm",(void*)f_877},
{"f_961:genequal_2escm",(void*)f_961},
{"f_819:genequal_2escm",(void*)f_819},
{"f_770:genequal_2escm",(void*)f_770},
{NULL,NULL}};
#endif

static C_PTABLE_ENTRY *create_ptable(void){
#ifdef C_ENABLE_PTABLES
return ptable;
#else
return NULL;
#endif
}

/*
o|hiding nonexported module bindings: genequal#count-up 
o|hiding nonexported module bindings: genequal#count-down 
o|hiding nonexported module bindings: genequal#make-bytevector 
o|hiding nonexported module bindings: genequal#bytevector 
o|hiding nonexported module bindings: genequal#bytevector? 
o|hiding nonexported module bindings: genequal#bytevector-u8-ref 
o|hiding nonexported module bindings: genequal#bytevector-u8-set! 
o|hiding nonexported module bindings: genequal#bytevector-length 
o|hiding nonexported module bindings: genequal#bytevector-copy 
o|hiding nonexported module bindings: genequal#bytevector-for-each 
o|safe globals: (genequal#make-specific-equality genequal#make-atomic-comparator genequal#generalized-equal? genequal#bytevector-for-each genequal#bytevector-copy genequal#bytevector-length genequal#bytevector-u8-set! genequal#bytevector-u8-ref genequal#bytevector? genequal#bytevector genequal#make-bytevector) 
o|Removed `not' forms: 7 
o|removed side-effect free assignment to unused variable: genequal#make-bytevector 
o|removed side-effect free assignment to unused variable: genequal#bytevector 
o|removed side-effect free assignment to unused variable: genequal#bytevector-u8-set! 
o|removed side-effect free assignment to unused variable: genequal#bytevector-copy 
o|removed side-effect free assignment to unused variable: genequal#bytevector-for-each 
o|substituted constant variable: from244250 
o|substituted constant variable: from270276 
o|replaced variables: 73 
o|removed binding forms: 22 
o|removed side-effect free assignment to unused variable: genequal#bytevector-u8-ref 
o|removed side-effect free assignment to unused variable: genequal#bytevector-length 
o|removed binding forms: 56 
o|removed binding forms: 2 
o|simplifications: ((##core#call . 45) (if . 2)) 
o|  call simplifications:
o|    vector?	2
o|    =	3
o|    vector-length	3
o|    >=	2
o|    vector-ref	2
o|    +	2
o|    pair?	2
o|    apply	3
o|    not	6
o|    null?	5
o|    eqv?	4
o|    car	4
o|    cdr	3
o|    boolean?	4
o|contracted procedure: k604 
o|contracted procedure: k616 
o|contracted procedure: k665 
o|contracted procedure: k671 
o|contracted procedure: k677 
o|contracted procedure: k683 
o|contracted procedure: k619 
o|contracted procedure: k627 
o|contracted procedure: k633 
o|contracted procedure: k639 
o|contracted procedure: k646 
o|contracted procedure: k652 
o|contracted procedure: k697 
o|contracted procedure: k792 
o|contracted procedure: k738 
o|contracted procedure: k744 
o|contracted procedure: k750 
o|contracted procedure: k756 
o|contracted procedure: k762 
o|contracted procedure: k775 
o|contracted procedure: k778 
o|contracted procedure: k781 
o|contracted procedure: k784 
o|contracted procedure: k874 
o|contracted procedure: k804 
o|contracted procedure: k862 
o|contracted procedure: k865 
o|contracted procedure: k812 
o|contracted procedure: k821 
o|contracted procedure: k832 
o|contracted procedure: k841 
o|contracted procedure: k853 
o|contracted procedure: k856 
o|contracted procedure: k871 
o|contracted procedure: k882 
o|contracted procedure: k890 
o|contracted procedure: k910 
o|contracted procedure: k919 
o|contracted procedure: k921 
o|simplifications: ((let . 15)) 
o|removed binding forms: 39 
o|customizable procedures: (k884 doloop278279 k806 doloop252253 loop223 k699 loop188) 
o|calls to known targets: 16 
o|fast box initializations: 4 
o|fast global references: 2 
o|fast global assignments: 1 
*/
/* end of file */
