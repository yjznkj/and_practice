
// Copyright 2013 the V8 project authors. All rights reserved.
    2 // Redistribution and use in source and binary forms, with or without
    3 // modification, are permitted provided that the following conditions are
    4 // met:
    5 //
    6 //     * Redistributions of source code must retain the above copyright
    7 //       notice, this list of conditions and the following disclaimer.
    8 //     * Redistributions in binary form must reproduce the above
    9 //       copyright notice, this list of conditions and the following
   10 //       disclaimer in the documentation and/or other materials provided
   11 //       with the distribution.
   12 //     * Neither the name of Google Inc. nor the names of its
   13 //       contributors may be used to endorse or promote products derived
   14 //       from this software without specific prior written permission.
   15 //
   16 // THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   17 // "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   18 // LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   19 // A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   20 // OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   21 // SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   22 // LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   23 // DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   24 // THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   25 // (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   26 // OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
   27 
   28 #ifndef V8_ARM64_MACRO_ASSEMBLER_ARM64_H_
   29 #define V8_ARM64_MACRO_ASSEMBLER_ARM64_H_
   30 
   31 #include <vector>
   32 
   33 #include "v8globals.h"
   34 #include "globals.h"
   35 
   36 #include "arm64/assembler-arm64-inl.h"
   37 
   38 namespace v8 {
   39 namespace internal {
   40 
   41 #define LS_MACRO_LIST(V)                                      \
   42   V(Ldrb, Register&, rt, LDRB_w)                              \
   43   V(Strb, Register&, rt, STRB_w)                              \
   44   V(Ldrsb, Register&, rt, rt.Is64Bits() ? LDRSB_x : LDRSB_w)  \
   45   V(Ldrh, Register&, rt, LDRH_w)                              \
   46   V(Strh, Register&, rt, STRH_w)                              \
   47   V(Ldrsh, Register&, rt, rt.Is64Bits() ? LDRSH_x : LDRSH_w)  \
   48   V(Ldr, CPURegister&, rt, LoadOpFor(rt))                     \
   49   V(Str, CPURegister&, rt, StoreOpFor(rt))                    \
   50   V(Ldrsw, Register&, rt, LDRSW_x)
   51 
   52 
   53 // ----------------------------------------------------------------------------
   54 // Static helper functions
   55 
   56 // Generate a MemOperand for loading a field from an object.
   57 inline MemOperand FieldMemOperand(Register object, int offset);
   58 inline MemOperand UntagSmiFieldMemOperand(Register object, int offset);
   59 
   60 // Generate a MemOperand for loading a SMI from memory.
   61 inline MemOperand UntagSmiMemOperand(Register object, int offset);
   62 
   63 
   64 // ----------------------------------------------------------------------------
   65 // MacroAssembler
   66 
   67 enum BranchType {
   68   // Copies of architectural conditions.
   69   // The associated conditions can be used in place of those, the code will
   70   // take care of reinterpreting them with the correct type.
   71   integer_eq = eq,
   72   integer_ne = ne,
   73   integer_hs = hs,
   74   integer_lo = lo,
   75   integer_mi = mi,
   76   integer_pl = pl,
   77   integer_vs = vs,
   78   integer_vc = vc,
   79   integer_hi = hi,
   80   integer_ls = ls,
   81   integer_ge = ge,
   82   integer_lt = lt,
   83   integer_gt = gt,
   84   integer_le = le,
   85   integer_al = al,
   86   integer_nv = nv,
   87 
   88   // These two are *different* from the architectural codes al and nv.
   89   // 'always' is used to generate unconditional branches.
   90   // 'never' is used to not generate a branch (generally as the inverse
   91   // branch type of 'always).
   92   always, never,
   93   // cbz and cbnz
   94   reg_zero, reg_not_zero,
   95   // tbz and tbnz
   96   reg_bit_clear, reg_bit_set,
   97 
   98   // Aliases.
   99   kBranchTypeFirstCondition = eq,
  100   kBranchTypeLastCondition = nv,
  101   kBranchTypeFirstUsingReg = reg_zero,
  102   kBranchTypeFirstUsingBit = reg_bit_clear
  103 };
  104 
  105 inline BranchType InvertBranchType(BranchType type) {
  106   if (kBranchTypeFirstCondition <= type && type <= kBranchTypeLastCondition) {
  107     return static_cast<BranchType>(
  108         InvertCondition(static_cast<Condition>(type)));
  109   } else {
  110     return static_cast<BranchType>(type ^ 1);
  111   }
  112 }
  113 
  114 enum RememberedSetAction { EMIT_REMEMBERED_SET, OMIT_REMEMBERED_SET };
  115 enum SmiCheck { INLINE_SMI_CHECK, OMIT_SMI_CHECK };
  116 enum LinkRegisterStatus { kLRHasNotBeenSaved, kLRHasBeenSaved };
  117 enum TargetAddressStorageMode {
  118   CAN_INLINE_TARGET_ADDRESS,
  119   NEVER_INLINE_TARGET_ADDRESS
  120 };
  121 enum UntagMode { kNotSpeculativeUntag, kSpeculativeUntag };
  122 enum ArrayHasHoles { kArrayCantHaveHoles, kArrayCanHaveHoles };
  123 enum CopyHint { kCopyUnknown, kCopyShort, kCopyLong };
  124 enum DiscardMoveMode { kDontDiscardForSameWReg, kDiscardForSameWReg };
  125 enum SeqStringSetCharCheckIndexType { kIndexIsSmi, kIndexIsInteger32 };
  126 
  127 class MacroAssembler : public Assembler {
  128  public:
  129   MacroAssembler(Isolate* isolate, byte * buffer, unsigned buffer_size);
  130 
  131   inline Handle<Object> CodeObject();
  132 
  133   // Instruction set functions ------------------------------------------------
  134   // Logical macros.
  135   inline void And(const Register& rd,
  136                   const Register& rn,
  137                   const Operand& operand);
  138   inline void Ands(const Register& rd,
  139                    const Register& rn,
  140                    const Operand& operand);
  141   inline void Bic(const Register& rd,
  142                   const Register& rn,
  143                   const Operand& operand);
  144   inline void Bics(const Register& rd,
  145                    const Register& rn,
  146                    const Operand& operand);
  147   inline void Orr(const Register& rd,
  148                   const Register& rn,
  149                   const Operand& operand);
  150   inline void Orn(const Register& rd,
  151                   const Register& rn,
  152                   const Operand& operand);
  153   inline void Eor(const Register& rd,
  154                   const Register& rn,
  155                   const Operand& operand);
  156   inline void Eon(const Register& rd,
  157                   const Register& rn,
  158                   const Operand& operand);
  159   inline void Tst(const Register& rn, const Operand& operand);
  160   void LogicalMacro(const Register& rd,
  161                     const Register& rn,
  162                     const Operand& operand,
  163                     LogicalOp op);
  164 
  165   // Add and sub macros.
  166   inline void Add(const Register& rd,
  167                   const Register& rn,
  168                   const Operand& operand);
  169   inline void Adds(const Register& rd,
  170                    const Register& rn,
  171                    const Operand& operand);
  172   inline void Sub(const Register& rd,
  173                   const Register& rn,
  174                   const Operand& operand);
  175   inline void Subs(const Register& rd,
  176                    const Register& rn,
  177                    const Operand& operand);
  178   inline void Cmn(const Register& rn, const Operand& operand);
  179   inline void Cmp(const Register& rn, const Operand& operand);
  180   inline void Neg(const Register& rd,
  181                   const Operand& operand);
  182   inline void Negs(const Register& rd,
  183                    const Operand& operand);
  184 
  185   void AddSubMacro(const Register& rd,
  186                    const Register& rn,
  187                    const Operand& operand,
  188                    FlagsUpdate S,
  189                    AddSubOp op);
  190 
  191   // Add/sub with carry macros.
  192   inline void Adc(const Register& rd,
  193                   const Register& rn,
  194                   const Operand& operand);
  195   inline void Adcs(const Register& rd,
  196                    const Register& rn,
  197                    const Operand& operand);
  198   inline void Sbc(const Register& rd,
  199                   const Register& rn,
  200                   const Operand& operand);
  201   inline void Sbcs(const Register& rd,
  202                    const Register& rn,
  203                    const Operand& operand);
  204   inline void Ngc(const Register& rd,
  205                   const Operand& operand);
  206   inline void Ngcs(const Register& rd,
  207                    const Operand& operand);
  208   void AddSubWithCarryMacro(const Register& rd,
  209                             const Register& rn,
  210                             const Operand& operand,
  211                             FlagsUpdate S,
  212                             AddSubWithCarryOp op);
  213 
  214   // Move macros.
  215   void Mov(const Register& rd,
  216            const Operand& operand,
  217            DiscardMoveMode discard_mode = kDontDiscardForSameWReg);
  218   void Mov(const Register& rd, uint64_t imm);
  219   inline void Mvn(const Register& rd, uint64_t imm);
  220   void Mvn(const Register& rd, const Operand& operand);
  221   static bool IsImmMovn(uint64_t imm, unsigned reg_size);
  222   static bool IsImmMovz(uint64_t imm, unsigned reg_size);
  223   static unsigned CountClearHalfWords(uint64_t imm, unsigned reg_size);
  224 
  225   // Conditional macros.
  226   inline void Ccmp(const Register& rn,
  227                    const Operand& operand,
  228                    StatusFlags nzcv,
  229                    Condition cond);
  230   inline void Ccmn(const Register& rn,
  231                    const Operand& operand,
  232                    StatusFlags nzcv,
  233                    Condition cond);
  234   void ConditionalCompareMacro(const Register& rn,
  235                                const Operand& operand,
  236                                StatusFlags nzcv,
  237                                Condition cond,
  238                                ConditionalCompareOp op);
  239   void Csel(const Register& rd,
  240             const Register& rn,
  241             const Operand& operand,
  242             Condition cond);
  243 
  244   // Load/store macros.
  245 #define DECLARE_FUNCTION(FN, REGTYPE, REG, OP) \
  246   inline void FN(const REGTYPE REG, const MemOperand& addr);
  247   LS_MACRO_LIST(DECLARE_FUNCTION)
  248 #undef DECLARE_FUNCTION
  249 
  250   void LoadStoreMacro(const CPURegister& rt,
  251                       const MemOperand& addr,
  252                       LoadStoreOp op);
  253 
  254   // V8-specific load/store helpers.
  255   void Load(const Register& rt, const MemOperand& addr, Representation r);
  256   void Store(const Register& rt, const MemOperand& addr, Representation r);
  257 
  258   // Remaining instructions are simple pass-through calls to the assembler.
  259   inline void Adr(const Register& rd, Label* label);
  260   inline void Asr(const Register& rd, const Register& rn, unsigned shift);
  261   inline void Asr(const Register& rd, const Register& rn, const Register& rm);
  262 
  263   // Branch type inversion relies on these relations.
  264   STATIC_ASSERT((reg_zero      == (reg_not_zero ^ 1)) &&
  265                 (reg_bit_clear == (reg_bit_set ^ 1)) &&
  266                 (always        == (never ^ 1)));
  267 
  268   void B(Label* label, BranchType type, Register reg = NoReg, int bit = -1);
  269 
  270   inline void B(Label* label);
  271   inline void B(Condition cond, Label* label);
  272   void B(Label* label, Condition cond);
  273   inline void Bfi(const Register& rd,
  274                   const Register& rn,
  275                   unsigned lsb,
  276                   unsigned width);
  277   inline void Bfxil(const Register& rd,
  278                     const Register& rn,
  279                     unsigned lsb,
  280                     unsigned width);
  281   inline void Bind(Label* label);
  282   inline void Bl(Label* label);
  283   inline void Blr(const Register& xn);
  284   inline void Br(const Register& xn);
  285   inline void Brk(int code);
  286   void Cbnz(const Register& rt, Label* label);
  287   void Cbz(const Register& rt, Label* label);
  288   inline void Cinc(const Register& rd, const Register& rn, Condition cond);
  289   inline void Cinv(const Register& rd, const Register& rn, Condition cond);
  290   inline void Cls(const Register& rd, const Register& rn);
  291   inline void Clz(const Register& rd, const Register& rn);
  292   inline void Cneg(const Register& rd, const Register& rn, Condition cond);
  293   inline void CzeroX(const Register& rd, Condition cond);
  294   inline void CmovX(const Register& rd, const Register& rn, Condition cond);
  295   inline void Cset(const Register& rd, Condition cond);
  296   inline void Csetm(const Register& rd, Condition cond);
  297   inline void Csinc(const Register& rd,
  298                     const Register& rn,
  299                     const Register& rm,
  300                     Condition cond);
  301   inline void Csinv(const Register& rd,
  302                     const Register& rn,
  303                     const Register& rm,
  304                     Condition cond);
  305   inline void Csneg(const Register& rd,
  306                     const Register& rn,
  307                     const Register& rm,
  308                     Condition cond);
  309   inline void Dmb(BarrierDomain domain, BarrierType type);
  310   inline void Dsb(BarrierDomain domain, BarrierType type);
  311   inline void Debug(const char* message, uint32_t code, Instr params = BREAK);
  312   inline void Extr(const Register& rd,
  313                    const Register& rn,
  314                    const Register& rm,
  315                    unsigned lsb);
  316   inline void Fabs(const FPRegister& fd, const FPRegister& fn);
  317   inline void Fadd(const FPRegister& fd,
  318                    const FPRegister& fn,
  319                    const FPRegister& fm);
  320   inline void Fccmp(const FPRegister& fn,
  321                     const FPRegister& fm,
  322                     StatusFlags nzcv,
  323                     Condition cond);
  324   inline void Fcmp(const FPRegister& fn, const FPRegister& fm);
  325   inline void Fcmp(const FPRegister& fn, double value);
  326   inline void Fcsel(const FPRegister& fd,
  327                     const FPRegister& fn,
  328                     const FPRegister& fm,
  329                     Condition cond);
  330   inline void Fcvt(const FPRegister& fd, const FPRegister& fn);
  331   inline void Fcvtas(const Register& rd, const FPRegister& fn);
  332   inline void Fcvtau(const Register& rd, const FPRegister& fn);
  333   inline void Fcvtms(const Register& rd, const FPRegister& fn);
  334   inline void Fcvtmu(const Register& rd, const FPRegister& fn);
  335   inline void Fcvtns(const Register& rd, const FPRegister& fn);
  336   inline void Fcvtnu(const Register& rd, const FPRegister& fn);
  337   inline void Fcvtzs(const Register& rd, const FPRegister& fn);
  338   inline void Fcvtzu(const Register& rd, const FPRegister& fn);
  339   inline void Fdiv(const FPRegister& fd,
  340                    const FPRegister& fn,
  341                    const FPRegister& fm);
  342   inline void Fmadd(const FPRegister& fd,
  343                     const FPRegister& fn,
  344                     const FPRegister& fm,
  345                     const FPRegister& fa);
  346   inline void Fmax(const FPRegister& fd,
  347                    const FPRegister& fn,
  348                    const FPRegister& fm);
  349   inline void Fmaxnm(const FPRegister& fd,
  350                      const FPRegister& fn,
  351                      const FPRegister& fm);
  352   inline void Fmin(const FPRegister& fd,
  353                    const FPRegister& fn,
  354                    const FPRegister& fm);
  355   inline void Fminnm(const FPRegister& fd,
  356                      const FPRegister& fn,
  357                      const FPRegister& fm);
  358   inline void Fmov(FPRegister fd, FPRegister fn);
  359   inline void Fmov(FPRegister fd, Register rn);
  360   // Provide explicit double and float interfaces for FP immediate moves, rather
  361   // than relying on implicit C++ casts. This allows signalling NaNs to be
  362   // preserved when the immediate matches the format of fd. Most systems convert
  363   // signalling NaNs to quiet NaNs when converting between float and double.
  364   inline void Fmov(FPRegister fd, double imm);
  365   inline void Fmov(FPRegister fd, float imm);
  366   // Provide a template to allow other types to be converted automatically.
  367   template<typename T>
  368   void Fmov(FPRegister fd, T imm) {
  369     ASSERT(allow_macro_instructions_);
  370     Fmov(fd, static_cast<double>(imm));
  371   }
  372   inline void Fmov(Register rd, FPRegister fn);
  373   inline void Fmsub(const FPRegister& fd,
  374                     const FPRegister& fn,
  375                     const FPRegister& fm,
  376                     const FPRegister& fa);
  377   inline void Fmul(const FPRegister& fd,
  378                    const FPRegister& fn,
  379                    const FPRegister& fm);
  380   inline void Fneg(const FPRegister& fd, const FPRegister& fn);
  381   inline void Fnmadd(const FPRegister& fd,
  382                      const FPRegister& fn,
  383                      const FPRegister& fm,
  384                      const FPRegister& fa);
  385   inline void Fnmsub(const FPRegister& fd,
  386                      const FPRegister& fn,
  387                      const FPRegister& fm,
  388                      const FPRegister& fa);
  389   inline void Frinta(const FPRegister& fd, const FPRegister& fn);
  390   inline void Frintn(const FPRegister& fd, const FPRegister& fn);
  391   inline void Frintz(const FPRegister& fd, const FPRegister& fn);
  392   inline void Fsqrt(const FPRegister& fd, const FPRegister& fn);
  393   inline void Fsub(const FPRegister& fd,
  394                    const FPRegister& fn,
  395                    const FPRegister& fm);
  396   inline void Hint(SystemHint code);
  397   inline void Hlt(int code);
  398   inline void Isb();
  399   inline void Ldnp(const CPURegister& rt,
  400                    const CPURegister& rt2,
  401                    const MemOperand& src);
  402   inline void Ldp(const CPURegister& rt,
  403                   const CPURegister& rt2,
  404                   const MemOperand& src);
  405   inline void Ldpsw(const Register& rt,
  406                     const Register& rt2,
  407                     const MemOperand& src);
  408   // Provide both double and float interfaces for FP immediate loads, rather
  409   // than relying on implicit C++ casts. This allows signalling NaNs to be
  410   // preserved when the immediate matches the format of fd. Most systems convert
  411   // signalling NaNs to quiet NaNs when converting between float and double.
  412   inline void Ldr(const FPRegister& ft, double imm);
  413   inline void Ldr(const FPRegister& ft, float imm);
  414   inline void Ldr(const Register& rt, uint64_t imm);
  415   inline void Lsl(const Register& rd, const Register& rn, unsigned shift);
  416   inline void Lsl(const Register& rd, const Register& rn, const Register& rm);
  417   inline void Lsr(const Register& rd, const Register& rn, unsigned shift);
  418   inline void Lsr(const Register& rd, const Register& rn, const Register& rm);
  419   inline void Madd(const Register& rd,
  420                    const Register& rn,
  421                    const Register& rm,
  422                    const Register& ra);
  423   inline void Mneg(const Register& rd, const Register& rn, const Register& rm);
  424   inline void Mov(const Register& rd, const Register& rm);
  425   inline void Movk(const Register& rd, uint64_t imm, int shift = -1);
  426   inline void Mrs(const Register& rt, SystemRegister sysreg);
  427   inline void Msr(SystemRegister sysreg, const Register& rt);
  428   inline void Msub(const Register& rd,
  429                    const Register& rn,
  430                    const Register& rm,
  431                    const Register& ra);
  432   inline void Mul(const Register& rd, const Register& rn, const Register& rm);
  433   inline void Nop() { nop(); }
  434   inline void Rbit(const Register& rd, const Register& rn);
  435   inline void Ret(const Register& xn = lr);
  436   inline void Rev(const Register& rd, const Register& rn);
  437   inline void Rev16(const Register& rd, const Register& rn);
  438   inline void Rev32(const Register& rd, const Register& rn);
  439   inline void Ror(const Register& rd, const Register& rs, unsigned shift);
  440   inline void Ror(const Register& rd, const Register& rn, const Register& rm);
  441   inline void Sbfiz(const Register& rd,
  442                     const Register& rn,
  443                     unsigned lsb,
  444                     unsigned width);
  445   inline void Sbfx(const Register& rd,
  446                    const Register& rn,
  447                    unsigned lsb,
  448                    unsigned width);
  449   inline void Scvtf(const FPRegister& fd,
  450                     const Register& rn,
  451                     unsigned fbits = 0);
  452   inline void Sdiv(const Register& rd, const Register& rn, const Register& rm);
  453   inline void Smaddl(const Register& rd,
  454                      const Register& rn,
  455                      const Register& rm,
  456                      const Register& ra);
  457   inline void Smsubl(const Register& rd,
  458                      const Register& rn,
  459                      const Register& rm,
  460                      const Register& ra);
  461   inline void Smull(const Register& rd,
  462                     const Register& rn,
  463                     const Register& rm);
  464   inline void Smulh(const Register& rd,
  465                     const Register& rn,
  466                     const Register& rm);
  467   inline void Stnp(const CPURegister& rt,
  468                    const CPURegister& rt2,
  469                    const MemOperand& dst);
  470   inline void Stp(const CPURegister& rt,
  471                   const CPURegister& rt2,
  472                   const MemOperand& dst);
  473   inline void Sxtb(const Register& rd, const Register& rn);
  474   inline void Sxth(const Register& rd, const Register& rn);
  475   inline void Sxtw(const Register& rd, const Register& rn);
  476   void Tbnz(const Register& rt, unsigned bit_pos, Label* label);
  477   void Tbz(const Register& rt, unsigned bit_pos, Label* label);
  478   inline void Ubfiz(const Register& rd,
  479                     const Register& rn,
  480                     unsigned lsb,
  481                     unsigned width);
  482   inline void Ubfx(const Register& rd,
  483                    const Register& rn,
  484                    unsigned lsb,
  485                    unsigned width);
  486   inline void Ucvtf(const FPRegister& fd,
  487                     const Register& rn,
  488                     unsigned fbits = 0);
  489   inline void Udiv(const Register& rd, const Register& rn, const Register& rm);
  490   inline void Umaddl(const Register& rd,
  491                      const Register& rn,
  492                      const Register& rm,
  493                      const Register& ra);
  494   inline void Umsubl(const Register& rd,
  495                      const Register& rn,
  496                      const Register& rm,
  497                      const Register& ra);
  498   inline void Uxtb(const Register& rd, const Register& rn);
  499   inline void Uxth(const Register& rd, const Register& rn);
  500   inline void Uxtw(const Register& rd, const Register& rn);
  501 
  502   // Pseudo-instructions ------------------------------------------------------
  503 
  504   // Compute rd = abs(rm).
  505   // This function clobbers the condition flags.
  506   //
  507   // If rm is the minimum representable value, the result is not representable.
  508   // Handlers for each case can be specified using the relevant labels.
  509   void Abs(const Register& rd, const Register& rm,
  510            Label * is_not_representable = NULL,
  511            Label * is_representable = NULL);
  512 
  513   // Push or pop up to 4 registers of the same width to or from the stack,
  514   // using the current stack pointer as set by SetStackPointer.
  515   //
  516   // If an argument register is 'NoReg', all further arguments are also assumed
  517   // to be 'NoReg', and are thus not pushed or popped.
  518   //
  519   // Arguments are ordered such that "Push(a, b);" is functionally equivalent
  520   // to "Push(a); Push(b);".
  521   //
  522   // It is valid to push the same register more than once, and there is no
  523   // restriction on the order in which registers are specified.
  524   //
  525   // It is not valid to pop into the same register more than once in one
  526   // operation, not even into the zero register.
  527   //
  528   // If the current stack pointer (as set by SetStackPointer) is csp, then it
  529   // must be aligned to 16 bytes on entry and the total size of the specified
  530   // registers must also be a multiple of 16 bytes.
  531   //
  532   // Even if the current stack pointer is not the system stack pointer (csp),
  533   // Push (and derived methods) will still modify the system stack pointer in
  534   // order to comply with ABI rules about accessing memory below the system
  535   // stack pointer.
  536   //
  537   // Other than the registers passed into Pop, the stack pointer and (possibly)
  538   // the system stack pointer, these methods do not modify any other registers.
  539   void Push(const CPURegister& src0, const CPURegister& src1 = NoReg,
  540             const CPURegister& src2 = NoReg, const CPURegister& src3 = NoReg);
  541   void Push(const CPURegister& src0, const CPURegister& src1,
  542             const CPURegister& src2, const CPURegister& src3,
  543             const CPURegister& src4, const CPURegister& src5 = NoReg,
  544             const CPURegister& src6 = NoReg, const CPURegister& src7 = NoReg);
  545   void Pop(const CPURegister& dst0, const CPURegister& dst1 = NoReg,
  546            const CPURegister& dst2 = NoReg, const CPURegister& dst3 = NoReg);
  547 
  548   // Alternative forms of Push and Pop, taking a RegList or CPURegList that
  549   // specifies the registers that are to be pushed or popped. Higher-numbered
  550   // registers are associated with higher memory addresses (as in the A32 push
  551   // and pop instructions).
  552   //
  553   // (Push|Pop)SizeRegList allow you to specify the register size as a
  554   // parameter. Only kXRegSizeInBits, kWRegSizeInBits, kDRegSizeInBits and
  555   // kSRegSizeInBits are supported.
  556   //
  557   // Otherwise, (Push|Pop)(CPU|X|W|D|S)RegList is preferred.
  558   void PushCPURegList(CPURegList registers);
  559   void PopCPURegList(CPURegList registers);
  560 
  561   inline void PushSizeRegList(RegList registers, unsigned reg_size,
  562       CPURegister::RegisterType type = CPURegister::kRegister) {
  563     PushCPURegList(CPURegList(type, reg_size, registers));
  564   }
  565   inline void PopSizeRegList(RegList registers, unsigned reg_size,
  566       CPURegister::RegisterType type = CPURegister::kRegister) {
  567     PopCPURegList(CPURegList(type, reg_size, registers));
  568   }
  569   inline void PushXRegList(RegList regs) {
  570     PushSizeRegList(regs, kXRegSizeInBits);
  571   }
  572   inline void PopXRegList(RegList regs) {
  573     PopSizeRegList(regs, kXRegSizeInBits);
  574   }
  575   inline void PushWRegList(RegList regs) {
  576     PushSizeRegList(regs, kWRegSizeInBits);
  577   }
  578   inline void PopWRegList(RegList regs) {
  579     PopSizeRegList(regs, kWRegSizeInBits);
  580   }
  581   inline void PushDRegList(RegList regs) {
  582     PushSizeRegList(regs, kDRegSizeInBits, CPURegister::kFPRegister);
  583   }
  584   inline void PopDRegList(RegList regs) {
  585     PopSizeRegList(regs, kDRegSizeInBits, CPURegister::kFPRegister);
  586   }
  587   inline void PushSRegList(RegList regs) {
  588     PushSizeRegList(regs, kSRegSizeInBits, CPURegister::kFPRegister);
  589   }
  590   inline void PopSRegList(RegList regs) {
  591     PopSizeRegList(regs, kSRegSizeInBits, CPURegister::kFPRegister);
  592   }
  593 
  594   // Push the specified register 'count' times.
  595   void PushMultipleTimes(CPURegister src, Register count);
  596   void PushMultipleTimes(CPURegister src, int count);
  597 
  598   // This is a convenience method for pushing a single Handle<Object>.
  599   inline void Push(Handle<Object> handle);
  600   void Push(Smi* smi) { Push(Handle<Smi>(smi, isolate())); }
  601 
  602   // Aliases of Push and Pop, required for V8 compatibility.
  603   inline void push(Register src) {
  604     Push(src);
  605   }
  606   inline void pop(Register dst) {
  607     Pop(dst);
  608   }
  609 
  610   // Sometimes callers need to push or pop multiple registers in a way that is
  611   // difficult to structure efficiently for fixed Push or Pop calls. This scope
  612   // allows push requests to be queued up, then flushed at once. The
  613   // MacroAssembler will try to generate the most efficient sequence required.
  614   //
  615   // Unlike the other Push and Pop macros, PushPopQueue can handle mixed sets of
  616   // register sizes and types.
  617   class PushPopQueue {
  618    public:
  619     explicit PushPopQueue(MacroAssembler* masm) : masm_(masm), size_(0) { }
  620 
  621     ~PushPopQueue() {
  622       ASSERT(queued_.empty());
  623     }
  624 
  625     void Queue(const CPURegister& rt) {
  626       size_ += rt.SizeInBytes();
  627       queued_.push_back(rt);
  628     }
  629 
  630     void PushQueued();
  631     void PopQueued();
  632 
  633    private:
  634     MacroAssembler* masm_;
  635     int size_;
  636     std::vector<CPURegister> queued_;
  637   };
  638 
  639   // Poke 'src' onto the stack. The offset is in bytes.
  640   //
  641   // If the current stack pointer (according to StackPointer()) is csp, then
  642   // csp must be aligned to 16 bytes.
  643   void Poke(const CPURegister& src, const Operand& offset);
  644 
  645   // Peek at a value on the stack, and put it in 'dst'. The offset is in bytes.
  646   //
  647   // If the current stack pointer (according to StackPointer()) is csp, then
  648   // csp must be aligned to 16 bytes.
  649   void Peek(const CPURegister& dst, const Operand& offset);
  650 
  651   // Poke 'src1' and 'src2' onto the stack. The values written will be adjacent
  652   // with 'src2' at a higher address than 'src1'. The offset is in bytes.
  653   //
  654   // If the current stack pointer (according to StackPointer()) is csp, then
  655   // csp must be aligned to 16 bytes.
  656   void PokePair(const CPURegister& src1, const CPURegister& src2, int offset);
  657 
  658   // Peek at two values on the stack, and put them in 'dst1' and 'dst2'. The
  659   // values peeked will be adjacent, with the value in 'dst2' being from a
  660   // higher address than 'dst1'. The offset is in bytes.
  661   //
  662   // If the current stack pointer (according to StackPointer()) is csp, then
  663   // csp must be aligned to 16 bytes.
  664   void PeekPair(const CPURegister& dst1, const CPURegister& dst2, int offset);
  665 
  666   // Claim or drop stack space without actually accessing memory.
  667   //
  668   // In debug mode, both of these will write invalid data into the claimed or
  669   // dropped space.
  670   //
  671   // If the current stack pointer (according to StackPointer()) is csp, then it
  672   // must be aligned to 16 bytes and the size claimed or dropped must be a
  673   // multiple of 16 bytes.
  674   //
  675   // Note that unit_size must be specified in bytes. For variants which take a
  676   // Register count, the unit size must be a power of two.
  677   inline void Claim(uint64_t count, uint64_t unit_size = kXRegSize);
  678   inline void Claim(const Register& count,
  679                     uint64_t unit_size = kXRegSize);
  680   inline void Drop(uint64_t count, uint64_t unit_size = kXRegSize);
  681   inline void Drop(const Register& count,
  682                    uint64_t unit_size = kXRegSize);
  683 
  684   // Variants of Claim and Drop, where the 'count' parameter is a SMI held in a
  685   // register.
  686   inline void ClaimBySMI(const Register& count_smi,
  687                          uint64_t unit_size = kXRegSize);
  688   inline void DropBySMI(const Register& count_smi,
  689                         uint64_t unit_size = kXRegSize);
  690 
  691   // Compare a register with an operand, and branch to label depending on the
  692   // condition. May corrupt the status flags.
  693   inline void CompareAndBranch(const Register& lhs,
  694                                const Operand& rhs,
  695                                Condition cond,
  696                                Label* label);
  697 
  698   // Test the bits of register defined by bit_pattern, and branch if ANY of
  699   // those bits are set. May corrupt the status flags.
  700   inline void TestAndBranchIfAnySet(const Register& reg,
  701                                     const uint64_t bit_pattern,
  702                                     Label* label);
  703 
  704   // Test the bits of register defined by bit_pattern, and branch if ALL of
  705   // those bits are clear (ie. not set.) May corrupt the status flags.
  706   inline void TestAndBranchIfAllClear(const Register& reg,
  707                                       const uint64_t bit_pattern,
  708                                       Label* label);
  709 
  710   // Insert one or more instructions into the instruction stream that encode
  711   // some caller-defined data. The instructions used will be executable with no
  712   // side effects.
  713   inline void InlineData(uint64_t data);
  714 
  715   // Insert an instrumentation enable marker into the instruction stream.
  716   inline void EnableInstrumentation();
  717 
  718   // Insert an instrumentation disable marker into the instruction stream.
  719   inline void DisableInstrumentation();
  720 
  721   // Insert an instrumentation event marker into the instruction stream. These
  722   // will be picked up by the instrumentation system to annotate an instruction
  723   // profile. The argument marker_name must be a printable two character string;
  724   // it will be encoded in the event marker.
  725   inline void AnnotateInstrumentation(const char* marker_name);
  726 
  727   // If emit_debug_code() is true, emit a run-time check to ensure that
  728   // StackPointer() does not point below the system stack pointer.
  729   //
  730   // Whilst it is architecturally legal for StackPointer() to point below csp,
  731   // it can be evidence of a potential bug because the ABI forbids accesses
  732   // below csp.
  733   //
  734   // If emit_debug_code() is false, this emits no code.
  735   //
  736   // If StackPointer() is the system stack pointer, this emits no code.
  737   void AssertStackConsistency();
  738 
  739   // Preserve the callee-saved registers (as defined by AAPCS64).
  740   //
  741   // Higher-numbered registers are pushed before lower-numbered registers, and
  742   // thus get higher addresses.
  743   // Floating-point registers are pushed before general-purpose registers, and
  744   // thus get higher addresses.
  745   //
  746   // Note that registers are not checked for invalid values. Use this method
  747   // only if you know that the GC won't try to examine the values on the stack.
  748   //
  749   // This method must not be called unless the current stack pointer (as set by
  750   // SetStackPointer) is the system stack pointer (csp), and is aligned to
  751   // ActivationFrameAlignment().
  752   void PushCalleeSavedRegisters();
  753 
  754   // Restore the callee-saved registers (as defined by AAPCS64).
  755   //
  756   // Higher-numbered registers are popped after lower-numbered registers, and
  757   // thus come from higher addresses.
  758   // Floating-point registers are popped after general-purpose registers, and
  759   // thus come from higher addresses.
  760   //
  761   // This method must not be called unless the current stack pointer (as set by
  762   // SetStackPointer) is the system stack pointer (csp), and is aligned to
  763   // ActivationFrameAlignment().
  764   void PopCalleeSavedRegisters();
  765 
  766   // Set the current stack pointer, but don't generate any code.
  767   inline void SetStackPointer(const Register& stack_pointer) {
  768     ASSERT(!TmpList()->IncludesAliasOf(stack_pointer));
  769     sp_ = stack_pointer;
  770   }
  771 
  772   // Return the current stack pointer, as set by SetStackPointer.
  773   inline const Register& StackPointer() const {
  774     return sp_;
  775   }
  776 
  777   // Align csp for a frame, as per ActivationFrameAlignment, and make it the
  778   // current stack pointer.
  779   inline void AlignAndSetCSPForFrame() {
  780     int sp_alignment = ActivationFrameAlignment();
  781     // AAPCS64 mandates at least 16-byte alignment.
  782     ASSERT(sp_alignment >= 16);
  783     ASSERT(IsPowerOf2(sp_alignment));
  784     Bic(csp, StackPointer(), sp_alignment - 1);
  785     SetStackPointer(csp);
  786   }
  787 
  788   // Push the system stack pointer (csp) down to allow the same to be done to
  789   // the current stack pointer (according to StackPointer()). This must be
  790   // called _before_ accessing the memory.
  791   //
  792   // This is necessary when pushing or otherwise adding things to the stack, to
  793   // satisfy the AAPCS64 constraint that the memory below the system stack
  794   // pointer is not accessed.
  795   //
  796   // This method asserts that StackPointer() is not csp, since the call does
  797   // not make sense in that context.
  798   inline void BumpSystemStackPointer(const Operand& space);
  799 
  800   // Helpers ------------------------------------------------------------------
  801   // Root register.
  802   inline void InitializeRootRegister();
  803 
  804   // Load an object from the root table.
  805   void LoadRoot(Register destination,
  806                 Heap::RootListIndex index);
  807   // Store an object to the root table.
  808   void StoreRoot(Register source,
  809                  Heap::RootListIndex index);
  810 
  811   // Load both TrueValue and FalseValue roots.
  812   void LoadTrueFalseRoots(Register true_root, Register false_root);
  813 
  814   void LoadHeapObject(Register dst, Handle<HeapObject> object);
  815 
  816   void LoadObject(Register result, Handle<Object> object) {
  817     AllowDeferredHandleDereference heap_object_check;
  818     if (object->IsHeapObject()) {
  819       LoadHeapObject(result, Handle<HeapObject>::cast(object));
  820     } else {
  821       ASSERT(object->IsSmi());
  822       Mov(result, Operand(object));
  823     }
  824   }
  825 
  826   static int SafepointRegisterStackIndex(int reg_code);
  827 
  828   // This is required for compatibility with architecture independant code.
  829   // Remove if not needed.
  830   inline void Move(Register dst, Register src) { Mov(dst, src); }
  831 
  832   void LoadInstanceDescriptors(Register map,
  833                                Register descriptors);
  834   void EnumLengthUntagged(Register dst, Register map);
  835   void EnumLengthSmi(Register dst, Register map);
  836   void NumberOfOwnDescriptors(Register dst, Register map);
  837 
  838   template<typename Field>
  839   void DecodeField(Register reg) {
  840     static const uint64_t shift = Field::kShift + kSmiShift;
  841     static const uint64_t setbits = CountSetBits(Field::kMask, 32);
  842     Ubfx(reg, reg, shift, setbits);
  843   }
  844 
  845   // ---- SMI and Number Utilities ----
  846 
  847   inline void SmiTag(Register dst, Register src);
  848   inline void SmiTag(Register smi);
  849   inline void SmiUntag(Register dst, Register src);
  850   inline void SmiUntag(Register smi);
  851   inline void SmiUntagToDouble(FPRegister dst,
  852                                Register src,
  853                                UntagMode mode = kNotSpeculativeUntag);
  854   inline void SmiUntagToFloat(FPRegister dst,
  855                               Register src,
  856                               UntagMode mode = kNotSpeculativeUntag);
  857 
  858   // Compute the absolute value of 'smi' and leave the result in 'smi'
  859   // register. If 'smi' is the most negative SMI, the absolute value cannot
  860   // be represented as a SMI and a jump to 'slow' is done.
  861   void SmiAbs(const Register& smi, Label* slow);
  862 
  863   inline void JumpIfSmi(Register value,
  864                         Label* smi_label,
  865                         Label* not_smi_label = NULL);
  866   inline void JumpIfNotSmi(Register value, Label* not_smi_label);
  867   inline void JumpIfBothSmi(Register value1,
  868                             Register value2,
  869                             Label* both_smi_label,
  870                             Label* not_smi_label = NULL);
  871   inline void JumpIfEitherSmi(Register value1,
  872                               Register value2,
  873                               Label* either_smi_label,
  874                               Label* not_smi_label = NULL);
  875   inline void JumpIfEitherNotSmi(Register value1,
  876                                  Register value2,
  877                                  Label* not_smi_label);
  878   inline void JumpIfBothNotSmi(Register value1,
  879                                Register value2,
  880                                Label* not_smi_label);
  881 
  882   // Abort execution if argument is a smi, enabled via --debug-code.
  883   void AssertNotSmi(Register object, BailoutReason reason = kOperandIsASmi);
  884   void AssertSmi(Register object, BailoutReason reason = kOperandIsNotASmi);
  885 
  886   // Abort execution if argument is not a name, enabled via --debug-code.
  887   void AssertName(Register object);
  888 
  889   // Abort execution if argument is not undefined or an AllocationSite, enabled
  890   // via --debug-code.
  891   void AssertUndefinedOrAllocationSite(Register object, Register scratch);
  892 
  893   // Abort execution if argument is not a string, enabled via --debug-code.
  894   void AssertString(Register object);
  895 
  896   void JumpForHeapNumber(Register object,
  897                          Register heap_number_map,
  898                          Label* on_heap_number,
  899                          Label* on_not_heap_number = NULL);
  900   void JumpIfHeapNumber(Register object,
  901                         Label* on_heap_number,
  902                         Register heap_number_map = NoReg);
  903   void JumpIfNotHeapNumber(Register object,
  904                            Label* on_not_heap_number,
  905                            Register heap_number_map = NoReg);
  906 
  907   // Sets the vs flag if the input is -0.0.
  908   void TestForMinusZero(DoubleRegister input);
  909 
  910   // Jump to label if the input double register contains -0.0.
  911   void JumpIfMinusZero(DoubleRegister input, Label* on_negative_zero);
  912 
  913   // Generate code to do a lookup in the number string cache. If the number in
  914   // the register object is found in the cache the generated code falls through
  915   // with the result in the result register. The object and the result register
  916   // can be the same. If the number is not found in the cache the code jumps to
  917   // the label not_found with only the content of register object unchanged.
  918   void LookupNumberStringCache(Register object,
  919                                Register result,
  920                                Register scratch1,
  921                                Register scratch2,
  922                                Register scratch3,
  923                                Label* not_found);
  924 
  925   // Saturate a signed 32-bit integer in input to an unsigned 8-bit integer in
  926   // output.
  927   void ClampInt32ToUint8(Register in_out);
  928   void ClampInt32ToUint8(Register output, Register input);
  929 
  930   // Saturate a double in input to an unsigned 8-bit integer in output.
  931   void ClampDoubleToUint8(Register output,
  932                           DoubleRegister input,
  933                           DoubleRegister dbl_scratch);
  934 
  935   // Try to convert a double to a signed 32-bit int.
  936   // This succeeds if the result compares equal to the input, so inputs of -0.0
  937   // are converted to 0 and handled as a success.
  938   //
  939   // On output the Z flag is set if the conversion was successful.
  940   void TryConvertDoubleToInt32(Register as_int,
  941                                FPRegister value,
  942                                FPRegister scratch_d,
  943                                Label* on_successful_conversion = NULL,
  944                                Label* on_failed_conversion = NULL) {
  945     ASSERT(as_int.Is32Bits());
  946     TryConvertDoubleToInt(as_int, value, scratch_d, on_successful_conversion,
  947                           on_failed_conversion);
  948   }
  949 
  950   // Try to convert a double to a signed 64-bit int.
  951   // This succeeds if the result compares equal to the input, so inputs of -0.0
  952   // are converted to 0 and handled as a success.
  953   //
  954   // On output the Z flag is set if the conversion was successful.
  955   void TryConvertDoubleToInt64(Register as_int,
  956                                FPRegister value,
  957                                FPRegister scratch_d,
  958                                Label* on_successful_conversion = NULL,
  959                                Label* on_failed_conversion = NULL) {
  960     ASSERT(as_int.Is64Bits());
  961     TryConvertDoubleToInt(as_int, value, scratch_d, on_successful_conversion,
  962                           on_failed_conversion);
  963   }
  964 
  965   // ---- Object Utilities ----
  966 
  967   // Copy fields from 'src' to 'dst', where both are tagged objects.
  968   // The 'temps' list is a list of X registers which can be used for scratch
  969   // values. The temps list must include at least one register.
  970   //
  971   // Currently, CopyFields cannot make use of more than three registers from
  972   // the 'temps' list.
  973   //
  974   // CopyFields expects to be able to take at least two registers from
  975   // MacroAssembler::TmpList().
  976   void CopyFields(Register dst, Register src, CPURegList temps, unsigned count);
  977 
  978   // Starting at address in dst, initialize field_count 64-bit fields with
  979   // 64-bit value in register filler. Register dst is corrupted.
  980   void FillFields(Register dst,
  981                   Register field_count,
  982                   Register filler);
  983 
  984   // Copies a number of bytes from src to dst. All passed registers are
  985   // clobbered. On exit src and dst will point to the place just after where the
  986   // last byte was read or written and length will be zero. Hint may be used to
  987   // determine which is the most efficient algorithm to use for copying.
  988   void CopyBytes(Register dst,
  989                  Register src,
  990                  Register length,
  991                  Register scratch,
  992                  CopyHint hint = kCopyUnknown);
  993 
  994   // ---- String Utilities ----
  995 
  996 
  997   // Jump to label if either object is not a sequential ASCII string.
  998   // Optionally perform a smi check on the objects first.
  999   void JumpIfEitherIsNotSequentialAsciiStrings(
 1000       Register first,
 1001       Register second,
 1002       Register scratch1,
 1003       Register scratch2,
 1004       Label* failure,
 1005       SmiCheckType smi_check = DO_SMI_CHECK);
 1006 
 1007   // Check if instance type is sequential ASCII string and jump to label if
 1008   // it is not.
 1009   void JumpIfInstanceTypeIsNotSequentialAscii(Register type,
 1010                                               Register scratch,
 1011                                               Label* failure);
 1012 
 1013   // Checks if both instance types are sequential ASCII strings and jumps to
 1014   // label if either is not.
 1015   void JumpIfEitherInstanceTypeIsNotSequentialAscii(
 1016       Register first_object_instance_type,
 1017       Register second_object_instance_type,
 1018       Register scratch1,
 1019       Register scratch2,
 1020       Label* failure);
 1021 
 1022   // Checks if both instance types are sequential ASCII strings and jumps to
 1023   // label if either is not.
 1024   void JumpIfBothInstanceTypesAreNotSequentialAscii(
 1025       Register first_object_instance_type,
 1026       Register second_object_instance_type,
 1027       Register scratch1,
 1028       Register scratch2,
 1029       Label* failure);
 1030 
 1031   void JumpIfNotUniqueName(Register type, Label* not_unique_name);
 1032 
 1033   // ---- Calling / Jumping helpers ----
 1034 
 1035   // This is required for compatibility in architecture indepenedant code.
 1036   inline void jmp(Label* L) { B(L); }
 1037 
 1038   // Passes thrown value to the handler of top of the try handler chain.
 1039   // Register value must be x0.
 1040   void Throw(Register value,
 1041              Register scratch1,
 1042              Register scratch2,
 1043              Register scratch3,
 1044              Register scratch4);
 1045 
 1046   // Propagates an uncatchable exception to the top of the current JS stack's
 1047   // handler chain. Register value must be x0.
 1048   void ThrowUncatchable(Register value,
 1049                         Register scratch1,
 1050                         Register scratch2,
 1051                         Register scratch3,
 1052                         Register scratch4);
 1053 
 1054   // Throw a message string as an exception.
 1055   void Throw(BailoutReason reason);
 1056 
 1057   // Throw a message string as an exception if a condition is not true.
 1058   void ThrowIf(Condition cc, BailoutReason reason);
 1059 
 1060   // Throw a message string as an exception if the value is a smi.
 1061   void ThrowIfSmi(const Register& value, BailoutReason reason);
 1062 
 1063   void CallStub(CodeStub* stub, TypeFeedbackId ast_id = TypeFeedbackId::None());
 1064   void TailCallStub(CodeStub* stub);
 1065 
 1066   void CallRuntime(const Runtime::Function* f,
 1067                    int num_arguments,
 1068                    SaveFPRegsMode save_doubles = kDontSaveFPRegs);
 1069 
 1070   void CallRuntime(Runtime::FunctionId id,
 1071                    int num_arguments,
 1072                    SaveFPRegsMode save_doubles = kDontSaveFPRegs) {
 1073     CallRuntime(Runtime::FunctionForId(id), num_arguments, save_doubles);
 1074   }
 1075 
 1076   void CallRuntimeSaveDoubles(Runtime::FunctionId id) {
 1077     const Runtime::Function* function = Runtime::FunctionForId(id);
 1078     CallRuntime(function, function->nargs, kSaveFPRegs);
 1079   }
 1080 
 1081   void TailCallRuntime(Runtime::FunctionId fid,
 1082                        int num_arguments,
 1083                        int result_size);
 1084 
 1085   int ActivationFrameAlignment();
 1086 
 1087   // Calls a C function.
 1088   // The called function is not allowed to trigger a
 1089   // garbage collection, since that might move the code and invalidate the
 1090   // return address (unless this is somehow accounted for by the called
 1091   // function).
 1092   void CallCFunction(ExternalReference function,
 1093                      int num_reg_arguments);
 1094   void CallCFunction(ExternalReference function,
 1095                      int num_reg_arguments,
 1096                      int num_double_arguments);
 1097   void CallCFunction(Register function,
 1098                      int num_reg_arguments,
 1099                      int num_double_arguments);
 1100 
 1101   // Calls an API function. Allocates HandleScope, extracts returned value
 1102   // from handle and propagates exceptions.
 1103   // 'stack_space' is the space to be unwound on exit (includes the call JS
 1104   // arguments space and the additional space allocated for the fast call).
 1105   // 'spill_offset' is the offset from the stack pointer where
 1106   // CallApiFunctionAndReturn can spill registers.
 1107   void CallApiFunctionAndReturn(Register function_address,
 1108                                 ExternalReference thunk_ref,
 1109                                 int stack_space,
 1110                                 int spill_offset,
 1111                                 MemOperand return_value_operand,
 1112                                 MemOperand* context_restore_operand);
 1113 
 1114   // The number of register that CallApiFunctionAndReturn will need to save on
 1115   // the stack. The space for these registers need to be allocated in the
 1116   // ExitFrame before calling CallApiFunctionAndReturn.
 1117   static const int kCallApiFunctionSpillSpace = 4;
 1118 
 1119   // Jump to a runtime routine.
 1120   void JumpToExternalReference(const ExternalReference& builtin);
 1121   // Tail call of a runtime routine (jump).
 1122   // Like JumpToExternalReference, but also takes care of passing the number
 1123   // of parameters.
 1124   void TailCallExternalReference(const ExternalReference& ext,
 1125                                  int num_arguments,
 1126                                  int result_size);
 1127   void CallExternalReference(const ExternalReference& ext,
 1128                              int num_arguments);
 1129 
 1130 
 1131   // Invoke specified builtin JavaScript function. Adds an entry to
 1132   // the unresolved list if the name does not resolve.
 1133   void InvokeBuiltin(Builtins::JavaScript id,
 1134                      InvokeFlag flag,
 1135                      const CallWrapper& call_wrapper = NullCallWrapper());
 1136 
 1137   // Store the code object for the given builtin in the target register and
 1138   // setup the function in the function register.
 1139   void GetBuiltinEntry(Register target,
 1140                        Register function,
 1141                        Builtins::JavaScript id);
 1142 
 1143   // Store the function for the given builtin in the target register.
 1144   void GetBuiltinFunction(Register target, Builtins::JavaScript id);
 1145 
 1146   void Jump(Register target);
 1147   void Jump(Address target, RelocInfo::Mode rmode);
 1148   void Jump(Handle<Code> code, RelocInfo::Mode rmode);
 1149   void Jump(intptr_t target, RelocInfo::Mode rmode);
 1150 
 1151   void Call(Register target);
 1152   void Call(Label* target);
 1153   void Call(Address target, RelocInfo::Mode rmode);
 1154   void Call(Handle<Code> code,
 1155             RelocInfo::Mode rmode = RelocInfo::CODE_TARGET,
 1156             TypeFeedbackId ast_id = TypeFeedbackId::None());
 1157 
 1158   // For every Call variant, there is a matching CallSize function that returns
 1159   // the size (in bytes) of the call sequence.
 1160   static int CallSize(Register target);
 1161   static int CallSize(Label* target);
 1162   static int CallSize(Address target, RelocInfo::Mode rmode);
 1163   static int CallSize(Handle<Code> code,
 1164                       RelocInfo::Mode rmode = RelocInfo::CODE_TARGET,
 1165                       TypeFeedbackId ast_id = TypeFeedbackId::None());
 1166 
 1167   // Registers used through the invocation chain are hard-coded.
 1168   // We force passing the parameters to ensure the contracts are correctly
 1169   // honoured by the caller.
 1170   // 'function' must be x1.
 1171   // 'actual' must use an immediate or x0.
 1172   // 'expected' must use an immediate or x2.
 1173   // 'call_kind' must be x5.
 1174   void InvokePrologue(const ParameterCount& expected,
 1175                       const ParameterCount& actual,
 1176                       Handle<Code> code_constant,
 1177                       Register code_reg,
 1178                       Label* done,
 1179                       InvokeFlag flag,
 1180                       bool* definitely_mismatches,
 1181                       const CallWrapper& call_wrapper);
 1182   void InvokeCode(Register code,
 1183                   const ParameterCount& expected,
 1184                   const ParameterCount& actual,
 1185                   InvokeFlag flag,
 1186                   const CallWrapper& call_wrapper);
 1187   // Invoke the JavaScript function in the given register.
 1188   // Changes the current context to the context in the function before invoking.
 1189   void InvokeFunction(Register function,
 1190                       const ParameterCount& actual,
 1191                       InvokeFlag flag,
 1192                       const CallWrapper& call_wrapper);
 1193   void InvokeFunction(Register function,
 1194                       const ParameterCount& expected,
 1195                       const ParameterCount& actual,
 1196                       InvokeFlag flag,
 1197                       const CallWrapper& call_wrapper);
 1198   void InvokeFunction(Handle<JSFunction> function,
 1199                       const ParameterCount& expected,
 1200                       const ParameterCount& actual,
 1201                       InvokeFlag flag,
 1202                       const CallWrapper& call_wrapper);
 1203 
 1204 
 1205   // ---- Floating point helpers ----
 1206 
 1207   // Perform a conversion from a double to a signed int64. If the input fits in
 1208   // range of the 64-bit result, execution branches to done. Otherwise,
 1209   // execution falls through, and the sign of the result can be used to
 1210   // determine if overflow was towards positive or negative infinity.
 1211   //
 1212   // On successful conversion, the least significant 32 bits of the result are
 1213   // equivalent to the ECMA-262 operation "ToInt32".
 1214   //
 1215   // Only public for the test code in test-code-stubs-arm64.cc.
 1216   void TryConvertDoubleToInt64(Register result,
 1217                                DoubleRegister input,
 1218                                Label* done);
 1219 
 1220   // Performs a truncating conversion of a floating point number as used by
 1221   // the JS bitwise operations. See ECMA-262 9.5: ToInt32.
 1222   // Exits with 'result' holding the answer.
 1223   void TruncateDoubleToI(Register result, DoubleRegister double_input);
 1224 
 1225   // Performs a truncating conversion of a heap number as used by
 1226   // the JS bitwise operations. See ECMA-262 9.5: ToInt32. 'result' and 'input'
 1227   // must be different registers.  Exits with 'result' holding the answer.
 1228   void TruncateHeapNumberToI(Register result, Register object);
 1229 
 1230   // Converts the smi or heap number in object to an int32 using the rules
 1231   // for ToInt32 as described in ECMAScript 9.5.: the value is truncated
 1232   // and brought into the range -2^31 .. +2^31 - 1. 'result' and 'input' must be
 1233   // different registers.
 1234   void TruncateNumberToI(Register object,
 1235                          Register result,
 1236                          Register heap_number_map,
 1237                          Label* not_int32);
 1238 
 1239   // ---- Code generation helpers ----
 1240 
 1241   void set_generating_stub(bool value) { generating_stub_ = value; }
 1242   bool generating_stub() const { return generating_stub_; }
 1243 #if DEBUG
 1244   void set_allow_macro_instructions(bool value) {
 1245     allow_macro_instructions_ = value;
 1246   }
 1247   bool allow_macro_instructions() const { return allow_macro_instructions_; }
 1248 #endif
 1249   bool use_real_aborts() const { return use_real_aborts_; }
 1250   void set_has_frame(bool value) { has_frame_ = value; }
 1251   bool has_frame() const { return has_frame_; }
 1252   bool AllowThisStubCall(CodeStub* stub);
 1253 
 1254   class NoUseRealAbortsScope {
 1255    public:
 1256     explicit NoUseRealAbortsScope(MacroAssembler* masm) :
 1257         saved_(masm->use_real_aborts_), masm_(masm) {
 1258       masm_->use_real_aborts_ = false;
 1259     }
 1260     ~NoUseRealAbortsScope() {
 1261       masm_->use_real_aborts_ = saved_;
 1262     }
 1263    private:
 1264     bool saved_;
 1265     MacroAssembler* masm_;
 1266   };
 1267 
 1268 #ifdef ENABLE_DEBUGGER_SUPPORT
 1269   // ---------------------------------------------------------------------------
 1270   // Debugger Support
 1271 
 1272   void DebugBreak();
 1273 #endif
 1274   // ---------------------------------------------------------------------------
 1275   // Exception handling
 1276 
 1277   // Push a new try handler and link into try handler chain.
 1278   void PushTryHandler(StackHandler::Kind kind, int handler_index);
 1279 
 1280   // Unlink the stack handler on top of the stack from the try handler chain.
 1281   // Must preserve the result register.
 1282   void PopTryHandler();
 1283 
 1284 
 1285   // ---------------------------------------------------------------------------
 1286   // Allocation support
 1287 
 1288   // Allocate an object in new space or old pointer space. The object_size is
 1289   // specified either in bytes or in words if the allocation flag SIZE_IN_WORDS
 1290   // is passed. The allocated object is returned in result.
 1291   //
 1292   // If the new space is exhausted control continues at the gc_required label.
 1293   // In this case, the result and scratch registers may still be clobbered.
 1294   // If flags includes TAG_OBJECT, the result is tagged as as a heap object.
 1295   void Allocate(Register object_size,
 1296                 Register result,
 1297                 Register scratch1,
 1298                 Register scratch2,
 1299                 Label* gc_required,
 1300                 AllocationFlags flags);
 1301 
 1302   void Allocate(int object_size,
 1303                 Register result,
 1304                 Register scratch1,
 1305                 Register scratch2,
 1306                 Label* gc_required,
 1307                 AllocationFlags flags);
 1308 
 1309   // Undo allocation in new space. The object passed and objects allocated after
 1310   // it will no longer be allocated. The caller must make sure that no pointers
 1311   // are left to the object(s) no longer allocated as they would be invalid when
 1312   // allocation is undone.
 1313   void UndoAllocationInNewSpace(Register object, Register scratch);
 1314 
 1315   void AllocateTwoByteString(Register result,
 1316                              Register length,
 1317                              Register scratch1,
 1318                              Register scratch2,
 1319                              Register scratch3,
 1320                              Label* gc_required);
 1321   void AllocateAsciiString(Register result,
 1322                            Register length,
 1323                            Register scratch1,
 1324                            Register scratch2,
 1325                            Register scratch3,
 1326                            Label* gc_required);
 1327   void AllocateTwoByteConsString(Register result,
 1328                                  Register length,
 1329                                  Register scratch1,
 1330                                  Register scratch2,
 1331                                  Label* gc_required);
 1332   void AllocateAsciiConsString(Register result,
 1333                                Register length,
 1334                                Register scratch1,
 1335                                Register scratch2,
 1336                                Label* gc_required);
 1337   void AllocateTwoByteSlicedString(Register result,
 1338                                    Register length,
 1339                                    Register scratch1,
 1340                                    Register scratch2,
 1341                                    Label* gc_required);
 1342   void AllocateAsciiSlicedString(Register result,
 1343                                  Register length,
 1344                                  Register scratch1,
 1345                                  Register scratch2,
 1346                                  Label* gc_required);
 1347 
 1348   // Allocates a heap number or jumps to the gc_required label if the young
 1349   // space is full and a scavenge is needed.
 1350   // All registers are clobbered.
 1351   // If no heap_number_map register is provided, the function will take care of
 1352   // loading it.
 1353   void AllocateHeapNumber(Register result,
 1354                           Label* gc_required,
 1355                           Register scratch1,
 1356                           Register scratch2,
 1357                           Register heap_number_map = NoReg);
 1358   void AllocateHeapNumberWithValue(Register result,
 1359                                    DoubleRegister value,
 1360                                    Label* gc_required,
 1361                                    Register scratch1,
 1362                                    Register scratch2,
 1363                                    Register heap_number_map = NoReg);
 1364 
 1365   // ---------------------------------------------------------------------------
 1366   // Support functions.
 1367 
 1368   // Try to get function prototype of a function and puts the value in the
 1369   // result register. Checks that the function really is a function and jumps
 1370   // to the miss label if the fast checks fail. The function register will be
 1371   // untouched; the other registers may be clobbered.
 1372   enum BoundFunctionAction {
 1373     kMissOnBoundFunction,
 1374     kDontMissOnBoundFunction
 1375   };
 1376 
 1377   void TryGetFunctionPrototype(Register function,
 1378                                Register result,
 1379                                Register scratch,
 1380                                Label* miss,
 1381                                BoundFunctionAction action =
 1382                                  kDontMissOnBoundFunction);
 1383 
 1384   // Compare object type for heap object.  heap_object contains a non-Smi
 1385   // whose object type should be compared with the given type.  This both
 1386   // sets the flags and leaves the object type in the type_reg register.
 1387   // It leaves the map in the map register (unless the type_reg and map register
 1388   // are the same register).  It leaves the heap object in the heap_object
 1389   // register unless the heap_object register is the same register as one of the
 1390   // other registers.
 1391   void CompareObjectType(Register heap_object,
 1392                          Register map,
 1393                          Register type_reg,
 1394                          InstanceType type);
 1395 
 1396 
 1397   // Compare object type for heap object, and branch if equal (or not.)
 1398   // heap_object contains a non-Smi whose object type should be compared with
 1399   // the given type.  This both sets the flags and leaves the object type in
 1400   // the type_reg register. It leaves the map in the map register (unless the
 1401   // type_reg and map register are the same register).  It leaves the heap
 1402   // object in the heap_object register unless the heap_object register is the
 1403   // same register as one of the other registers.
 1404   void JumpIfObjectType(Register object,
 1405                         Register map,
 1406                         Register type_reg,
 1407                         InstanceType type,
 1408                         Label* if_cond_pass,
 1409                         Condition cond = eq);
 1410 
 1411   void JumpIfNotObjectType(Register object,
 1412                            Register map,
 1413                            Register type_reg,
 1414                            InstanceType type,
 1415                            Label* if_not_object);
 1416 
 1417   // Compare instance type in a map.  map contains a valid map object whose
 1418   // object type should be compared with the given type.  This both
 1419   // sets the flags and leaves the object type in the type_reg register.
 1420   void CompareInstanceType(Register map,
 1421                            Register type_reg,
 1422                            InstanceType type);
 1423 
 1424   // Compare an object's map with the specified map. Condition flags are set
 1425   // with result of map compare.
 1426   void CompareMap(Register obj,
 1427                   Register scratch,
 1428                   Handle<Map> map);
 1429 
 1430   // As above, but the map of the object is already loaded into the register
 1431   // which is preserved by the code generated.
 1432   void CompareMap(Register obj_map,
 1433                   Handle<Map> map);
 1434 
 1435   // Check if the map of an object is equal to a specified map and branch to
 1436   // label if not. Skip the smi check if not required (object is known to be a
 1437   // heap object). If mode is ALLOW_ELEMENT_TRANSITION_MAPS, then also match
 1438   // against maps that are ElementsKind transition maps of the specified map.
 1439   void CheckMap(Register obj,
 1440                 Register scratch,
 1441                 Handle<Map> map,
 1442                 Label* fail,
 1443                 SmiCheckType smi_check_type);
 1444 
 1445 
 1446   void CheckMap(Register obj,
 1447                 Register scratch,
 1448                 Heap::RootListIndex index,
 1449                 Label* fail,
 1450                 SmiCheckType smi_check_type);
 1451 
 1452   // As above, but the map of the object is already loaded into obj_map, and is
 1453   // preserved.
 1454   void CheckMap(Register obj_map,
 1455                 Handle<Map> map,
 1456                 Label* fail,
 1457                 SmiCheckType smi_check_type);
 1458 
 1459   // Check if the map of an object is equal to a specified map and branch to a
 1460   // specified target if equal. Skip the smi check if not required (object is
 1461   // known to be a heap object)
 1462   void DispatchMap(Register obj,
 1463                    Register scratch,
 1464                    Handle<Map> map,
 1465                    Handle<Code> success,
 1466                    SmiCheckType smi_check_type);
 1467 
 1468   // Test the bitfield of the heap object map with mask and set the condition
 1469   // flags. The object register is preserved.
 1470   void TestMapBitfield(Register object, uint64_t mask);
 1471 
 1472   // Load the elements kind field from a map, and return it in the result
 1473   // register.
 1474   void LoadElementsKindFromMap(Register result, Register map);
 1475 
 1476   // Compare the object in a register to a value from the root list.
 1477   void CompareRoot(const Register& obj, Heap::RootListIndex index);
 1478 
 1479   // Compare the object in a register to a value and jump if they are equal.
 1480   void JumpIfRoot(const Register& obj,
 1481                   Heap::RootListIndex index,
 1482                   Label* if_equal);
 1483 
 1484   // Compare the object in a register to a value and jump if they are not equal.
 1485   void JumpIfNotRoot(const Register& obj,
 1486                      Heap::RootListIndex index,
 1487                      Label* if_not_equal);
 1488 
 1489   // Load and check the instance type of an object for being a unique name.
 1490   // Loads the type into the second argument register.
 1491   // The object and type arguments can be the same register; in that case it
 1492   // will be overwritten with the type.
 1493   // Fall-through if the object was a string and jump on fail otherwise.
 1494   inline void IsObjectNameType(Register object, Register type, Label* fail);
 1495 
 1496   inline void IsObjectJSObjectType(Register heap_object,
 1497                                    Register map,
 1498                                    Register scratch,
 1499                                    Label* fail);
 1500 
 1501   // Check the instance type in the given map to see if it corresponds to a
 1502   // JS object type. Jump to the fail label if this is not the case and fall
 1503   // through otherwise. However if fail label is NULL, no branch will be
 1504   // performed and the flag will be updated. You can test the flag for "le"
 1505   // condition to test if it is a valid JS object type.
 1506   inline void IsInstanceJSObjectType(Register map,
 1507                                      Register scratch,
 1508                                      Label* fail);
 1509 
 1510   // Load and check the instance type of an object for being a string.
 1511   // Loads the type into the second argument register.
 1512   // The object and type arguments can be the same register; in that case it
 1513   // will be overwritten with the type.
 1514   // Jumps to not_string or string appropriate. If the appropriate label is
 1515   // NULL, fall through.
 1516   inline void IsObjectJSStringType(Register object, Register type,
 1517                                    Label* not_string, Label* string = NULL);
 1518 
 1519   // Compare the contents of a register with an operand, and branch to true,
 1520   // false or fall through, depending on condition.
 1521   void CompareAndSplit(const Register& lhs,
 1522                        const Operand& rhs,
 1523                        Condition cond,
 1524                        Label* if_true,
 1525                        Label* if_false,
 1526                        Label* fall_through);
 1527 
 1528   // Test the bits of register defined by bit_pattern, and branch to
 1529   // if_any_set, if_all_clear or fall_through accordingly.
 1530   void TestAndSplit(const Register& reg,
 1531                     uint64_t bit_pattern,
 1532                     Label* if_all_clear,
 1533                     Label* if_any_set,
 1534                     Label* fall_through);
 1535 
 1536   // Check if a map for a JSObject indicates that the object has fast elements.
 1537   // Jump to the specified label if it does not.
 1538   void CheckFastElements(Register map, Register scratch, Label* fail);
 1539 
 1540   // Check if a map for a JSObject indicates that the object can have both smi
 1541   // and HeapObject elements.  Jump to the specified label if it does not.
 1542   void CheckFastObjectElements(Register map, Register scratch, Label* fail);
 1543 
 1544   // Check to see if number can be stored as a double in FastDoubleElements.
 1545   // If it can, store it at the index specified by key_reg in the array,
 1546   // otherwise jump to fail.
 1547   void StoreNumberToDoubleElements(Register value_reg,
 1548                                    Register key_reg,
 1549                                    Register elements_reg,
 1550                                    Register scratch1,
 1551                                    FPRegister fpscratch1,
 1552                                    FPRegister fpscratch2,
 1553                                    Label* fail,
 1554                                    int elements_offset = 0);
 1555 
 1556   // Picks out an array index from the hash field.
 1557   // Register use:
 1558   //   hash - holds the index's hash. Clobbered.
 1559   //   index - holds the overwritten index on exit.
 1560   void IndexFromHash(Register hash, Register index);
 1561 
 1562   // ---------------------------------------------------------------------------
 1563   // Inline caching support.
 1564 
 1565   void EmitSeqStringSetCharCheck(Register string,
 1566                                  Register index,
 1567                                  SeqStringSetCharCheckIndexType index_type,
 1568                                  Register scratch,
 1569                                  uint32_t encoding_mask);
 1570 
 1571   // Generate code for checking access rights - used for security checks
 1572   // on access to global objects across environments. The holder register
 1573   // is left untouched, whereas both scratch registers are clobbered.
 1574   void CheckAccessGlobalProxy(Register holder_reg,
 1575                               Register scratch1,
 1576                               Register scratch2,
 1577                               Label* miss);
 1578 
 1579   // Hash the interger value in 'key' register.
 1580   // It uses the same algorithm as ComputeIntegerHash in utils.h.
 1581   void GetNumberHash(Register key, Register scratch);
 1582 
 1583   // Load value from the dictionary.
 1584   //
 1585   // elements - holds the slow-case elements of the receiver on entry.
 1586   //            Unchanged unless 'result' is the same register.
 1587   //
 1588   // key      - holds the smi key on entry.
 1589   //            Unchanged unless 'result' is the same register.
 1590   //
 1591   // result   - holds the result on exit if the load succeeded.
 1592   //            Allowed to be the same as 'key' or 'result'.
 1593   //            Unchanged on bailout so 'key' or 'result' can be used
 1594   //            in further computation.
 1595   void LoadFromNumberDictionary(Label* miss,
 1596                                 Register elements,
 1597                                 Register key,
 1598                                 Register result,
 1599                                 Register scratch0,
 1600                                 Register scratch1,
 1601                                 Register scratch2,
 1602                                 Register scratch3);
 1603 
 1604   // ---------------------------------------------------------------------------
 1605   // Frames.
 1606 
 1607   // Activation support.
 1608   void EnterFrame(StackFrame::Type type);
 1609   void LeaveFrame(StackFrame::Type type);
 1610 
 1611   // Returns map with validated enum cache in object register.
 1612   void CheckEnumCache(Register object,
 1613                       Register null_value,
 1614                       Register scratch0,
 1615                       Register scratch1,
 1616                       Register scratch2,
 1617                       Register scratch3,
 1618                       Label* call_runtime);
 1619 
 1620   // AllocationMemento support. Arrays may have an associated
 1621   // AllocationMemento object that can be checked for in order to pretransition
 1622   // to another type.
 1623   // On entry, receiver should point to the array object.
 1624   // If allocation info is present, the Z flag is set (so that the eq
 1625   // condition will pass).
 1626   void TestJSArrayForAllocationMemento(Register receiver,
 1627                                        Register scratch1,
 1628                                        Register scratch2,
 1629                                        Label* no_memento_found);
 1630 
 1631   void JumpIfJSArrayHasAllocationMemento(Register receiver,
 1632                                          Register scratch1,
 1633                                          Register scratch2,
 1634                                          Label* memento_found) {
 1635     Label no_memento_found;
 1636     TestJSArrayForAllocationMemento(receiver, scratch1, scratch2,
 1637                                     &no_memento_found);
 1638     B(eq, memento_found);
 1639     Bind(&no_memento_found);
 1640   }
 1641 
 1642   // The stack pointer has to switch between csp and jssp when setting up and
 1643   // destroying the exit frame. Hence preserving/restoring the registers is
 1644   // slightly more complicated than simple push/pop operations.
 1645   void ExitFramePreserveFPRegs();
 1646   void ExitFrameRestoreFPRegs();
 1647 
 1648   // Generates function and stub prologue code.
 1649   void Prologue(PrologueFrameMode frame_mode);
 1650 
 1651   // Enter exit frame. Exit frames are used when calling C code from generated
 1652   // (JavaScript) code.
 1653   //
 1654   // The stack pointer must be jssp on entry, and will be set to csp by this
 1655   // function. The frame pointer is also configured, but the only other
 1656   // registers modified by this function are the provided scratch register, and
 1657   // jssp.
 1658   //
 1659   // The 'extra_space' argument can be used to allocate some space in the exit
 1660   // frame that will be ignored by the GC. This space will be reserved in the
 1661   // bottom of the frame immediately above the return address slot.
 1662   //
 1663   // Set up a stack frame and registers as follows:
 1664   //         fp[8]: CallerPC (lr)
 1665   //   fp -> fp[0]: CallerFP (old fp)
 1666   //         fp[-8]: SPOffset (new csp)
 1667   //         fp[-16]: CodeObject()
 1668   //         fp[-16 - fp-size]: Saved doubles, if saved_doubles is true.
 1669   //         csp[8]: Memory reserved for the caller if extra_space != 0.
 1670   //                 Alignment padding, if necessary.
 1671   //  csp -> csp[0]: Space reserved for the return address.
 1672   //
 1673   // This function also stores the new frame information in the top frame, so
 1674   // that the new frame becomes the current frame.
 1675   void EnterExitFrame(bool save_doubles,
 1676                       const Register& scratch,
 1677                       int extra_space = 0);
 1678 
 1679   // Leave the current exit frame, after a C function has returned to generated
 1680   // (JavaScript) code.
 1681   //
 1682   // This effectively unwinds the operation of EnterExitFrame:
 1683   //  * Preserved doubles are restored (if restore_doubles is true).
 1684   //  * The frame information is removed from the top frame.
 1685   //  * The exit frame is dropped.
 1686   //  * The stack pointer is reset to jssp.
 1687   //
 1688   // The stack pointer must be csp on entry.
 1689   void LeaveExitFrame(bool save_doubles,
 1690                       const Register& scratch,
 1691                       bool restore_context);
 1692 
 1693   void LoadContext(Register dst, int context_chain_length);
 1694 
 1695   // Emit code for a truncating division by a constant. The dividend register is
 1696   // unchanged. Dividend and result must be different.
 1697   void TruncatingDiv(Register result, Register dividend, int32_t divisor);
 1698 
 1699   // ---------------------------------------------------------------------------
 1700   // StatsCounter support
 1701 
 1702   void SetCounter(StatsCounter* counter, int value, Register scratch1,
 1703                   Register scratch2);
 1704   void IncrementCounter(StatsCounter* counter, int value, Register scratch1,
 1705                         Register scratch2);
 1706   void DecrementCounter(StatsCounter* counter, int value, Register scratch1,
 1707                         Register scratch2);
 1708 
 1709   // ---------------------------------------------------------------------------
 1710   // Garbage collector support (GC).
 1711 
 1712   enum RememberedSetFinalAction {
 1713     kReturnAtEnd,
 1714     kFallThroughAtEnd
 1715   };
 1716 
 1717   // Record in the remembered set the fact that we have a pointer to new space
 1718   // at the address pointed to by the addr register. Only works if addr is not
 1719   // in new space.
 1720   void RememberedSetHelper(Register object,  // Used for debug code.
 1721                            Register addr,
 1722                            Register scratch1,
 1723                            SaveFPRegsMode save_fp,
 1724                            RememberedSetFinalAction and_then);
 1725 
 1726   // Push and pop the registers that can hold pointers, as defined by the
 1727   // RegList constant kSafepointSavedRegisters.
 1728   void PushSafepointRegisters();
 1729   void PopSafepointRegisters();
 1730 
 1731   void PushSafepointRegistersAndDoubles();
 1732   void PopSafepointRegistersAndDoubles();
 1733 
 1734   // Store value in register src in the safepoint stack slot for register dst.
 1735   void StoreToSafepointRegisterSlot(Register src, Register dst) {
 1736     Poke(src, SafepointRegisterStackIndex(dst.code()) * kPointerSize);
 1737   }
 1738 
 1739   // Load the value of the src register from its safepoint stack slot
 1740   // into register dst.
 1741   void LoadFromSafepointRegisterSlot(Register dst, Register src) {
 1742     Peek(src, SafepointRegisterStackIndex(dst.code()) * kPointerSize);
 1743   }
 1744 
 1745   void CheckPageFlagSet(const Register& object,
 1746                         const Register& scratch,
 1747                         int mask,
 1748                         Label* if_any_set);
 1749 
 1750   void CheckPageFlagClear(const Register& object,
 1751                           const Register& scratch,
 1752                           int mask,
 1753                           Label* if_all_clear);
 1754 
 1755   void CheckMapDeprecated(Handle<Map> map,
 1756                           Register scratch,
 1757                           Label* if_deprecated);
 1758 
 1759   // Check if object is in new space and jump accordingly.
 1760   // Register 'object' is preserved.
 1761   void JumpIfNotInNewSpace(Register object,
 1762                            Label* branch) {
 1763     InNewSpace(object, ne, branch);
 1764   }
 1765 
 1766   void JumpIfInNewSpace(Register object,
 1767                         Label* branch) {
 1768     InNewSpace(object, eq, branch);
 1769   }
 1770 
 1771   // Notify the garbage collector that we wrote a pointer into an object.
 1772   // |object| is the object being stored into, |value| is the object being
 1773   // stored.  value and scratch registers are clobbered by the operation.
 1774   // The offset is the offset from the start of the object, not the offset from
 1775   // the tagged HeapObject pointer.  For use with FieldOperand(reg, off).
 1776   void RecordWriteField(
 1777       Register object,
 1778       int offset,
 1779       Register value,
 1780       Register scratch,
 1781       LinkRegisterStatus lr_status,
 1782       SaveFPRegsMode save_fp,
 1783       RememberedSetAction remembered_set_action = EMIT_REMEMBERED_SET,
 1784       SmiCheck smi_check = INLINE_SMI_CHECK);
 1785 
 1786   // As above, but the offset has the tag presubtracted. For use with
 1787   // MemOperand(reg, off).
 1788   inline void RecordWriteContextSlot(
 1789       Register context,
 1790       int offset,
 1791       Register value,
 1792       Register scratch,
 1793       LinkRegisterStatus lr_status,
 1794       SaveFPRegsMode save_fp,
 1795       RememberedSetAction remembered_set_action = EMIT_REMEMBERED_SET,
 1796       SmiCheck smi_check = INLINE_SMI_CHECK) {
 1797     RecordWriteField(context,
 1798                      offset + kHeapObjectTag,
 1799                      value,
 1800                      scratch,
 1801                      lr_status,
 1802                      save_fp,
 1803                      remembered_set_action,
 1804                      smi_check);
 1805   }
 1806 
 1807   // For a given |object| notify the garbage collector that the slot |address|
 1808   // has been written.  |value| is the object being stored. The value and
 1809   // address registers are clobbered by the operation.
 1810   void RecordWrite(
 1811       Register object,
 1812       Register address,
 1813       Register value,
 1814       LinkRegisterStatus lr_status,
 1815       SaveFPRegsMode save_fp,
 1816       RememberedSetAction remembered_set_action = EMIT_REMEMBERED_SET,
 1817       SmiCheck smi_check = INLINE_SMI_CHECK);
 1818 
 1819   // Checks the color of an object. If the object is already grey or black
 1820   // then we just fall through, since it is already live. If it is white and
 1821   // we can determine that it doesn't need to be scanned, then we just mark it
 1822   // black and fall through. For the rest we jump to the label so the
 1823   // incremental marker can fix its assumptions.
 1824   void EnsureNotWhite(Register object,
 1825                       Register scratch1,
 1826                       Register scratch2,
 1827                       Register scratch3,
 1828                       Register scratch4,
 1829                       Label* object_is_white_and_not_data);
 1830 
 1831   // Detects conservatively whether an object is data-only, i.e. it does need to
 1832   // be scanned by the garbage collector.
 1833   void JumpIfDataObject(Register value,
 1834                         Register scratch,
 1835                         Label* not_data_object);
 1836 
 1837   // Helper for finding the mark bits for an address.
 1838   // Note that the behaviour slightly differs from other architectures.
 1839   // On exit:
 1840   //  - addr_reg is unchanged.
 1841   //  - The bitmap register points at the word with the mark bits.
 1842   //  - The shift register contains the index of the first color bit for this
 1843   //    object in the bitmap.
 1844   inline void GetMarkBits(Register addr_reg,
 1845                           Register bitmap_reg,
 1846                           Register shift_reg);
 1847 
 1848   // Check if an object has a given incremental marking color.
 1849   void HasColor(Register object,
 1850                 Register scratch0,
 1851                 Register scratch1,
 1852                 Label* has_color,
 1853                 int first_bit,
 1854                 int second_bit);
 1855 
 1856   void JumpIfBlack(Register object,
 1857                    Register scratch0,
 1858                    Register scratch1,
 1859                    Label* on_black);
 1860 
 1861 
 1862   // Get the location of a relocated constant (its address in the constant pool)
 1863   // from its load site.
 1864   void GetRelocatedValueLocation(Register ldr_location,
 1865                                  Register result);
 1866 
 1867 
 1868   // ---------------------------------------------------------------------------
 1869   // Debugging.
 1870 
 1871   // Calls Abort(msg) if the condition cond is not satisfied.
 1872   // Use --debug_code to enable.
 1873   void Assert(Condition cond, BailoutReason reason);
 1874   void AssertRegisterIsClear(Register reg, BailoutReason reason);
 1875   void AssertRegisterIsRoot(
 1876       Register reg,
 1877       Heap::RootListIndex index,
 1878       BailoutReason reason = kRegisterDidNotMatchExpectedRoot);
 1879   void AssertFastElements(Register elements);
 1880 
 1881   // Abort if the specified register contains the invalid color bit pattern.
 1882   // The pattern must be in bits [1:0] of 'reg' register.
 1883   //
 1884   // If emit_debug_code() is false, this emits no code.
 1885   void AssertHasValidColor(const Register& reg);
 1886 
 1887   // Abort if 'object' register doesn't point to a string object.
 1888   //
 1889   // If emit_debug_code() is false, this emits no code.
 1890   void AssertIsString(const Register& object);
 1891 
 1892   // Like Assert(), but always enabled.
 1893   void Check(Condition cond, BailoutReason reason);
 1894   void CheckRegisterIsClear(Register reg, BailoutReason reason);
 1895 
 1896   // Print a message to stderr and abort execution.
 1897   void Abort(BailoutReason reason);
 1898 
 1899   // Conditionally load the cached Array transitioned map of type
 1900   // transitioned_kind from the native context if the map in register
 1901   // map_in_out is the cached Array map in the native context of
 1902   // expected_kind.
 1903   void LoadTransitionedArrayMapConditional(
 1904       ElementsKind expected_kind,
 1905       ElementsKind transitioned_kind,
 1906       Register map_in_out,
 1907       Register scratch1,
 1908       Register scratch2,
 1909       Label* no_map_match);
 1910 
 1911   void LoadGlobalFunction(int index, Register function);
 1912 
 1913   // Load the initial map from the global function. The registers function and
 1914   // map can be the same, function is then overwritten.
 1915   void LoadGlobalFunctionInitialMap(Register function,
 1916                                     Register map,
 1917                                     Register scratch);
 1918 
 1919   CPURegList* TmpList() { return &tmp_list_; }
 1920   CPURegList* FPTmpList() { return &fptmp_list_; }
 1921 
 1922   // Like printf, but print at run-time from generated code.
 1923   //
 1924   // The caller must ensure that arguments for floating-point placeholders
 1925   // (such as %e, %f or %g) are FPRegisters, and that arguments for integer
 1926   // placeholders are Registers.
 1927   //
 1928   // A maximum of four arguments may be given to any single Printf call. The
 1929   // arguments must be of the same type, but they do not need to have the same
 1930   // size.
 1931   //
 1932   // The following registers cannot be printed:
 1933   //    StackPointer(), csp.
 1934   //
 1935   // This function automatically preserves caller-saved registers so that
 1936   // calling code can use Printf at any point without having to worry about
 1937   // corruption. The preservation mechanism generates a lot of code. If this is
 1938   // a problem, preserve the important registers manually and then call
 1939   // PrintfNoPreserve. Callee-saved registers are not used by Printf, and are
 1940   // implicitly preserved.
 1941   //
 1942   // Unlike many MacroAssembler functions, x8 and x9 are guaranteed to be
 1943   // preserved, and can be printed. This allows Printf to be used during debug
 1944   // code.
 1945   //
 1946   // This function assumes (and asserts) that the current stack pointer is
 1947   // callee-saved, not caller-saved. This is most likely the case anyway, as a
 1948   // caller-saved stack pointer doesn't make a lot of sense.
 1949   void Printf(const char * format,
 1950               const CPURegister& arg0 = NoCPUReg,
 1951               const CPURegister& arg1 = NoCPUReg,
 1952               const CPURegister& arg2 = NoCPUReg,
 1953               const CPURegister& arg3 = NoCPUReg);
 1954 
 1955   // Like Printf, but don't preserve any caller-saved registers, not even 'lr'.
 1956   //
 1957   // The return code from the system printf call will be returned in x0.
 1958   void PrintfNoPreserve(const char * format,
 1959                         const CPURegister& arg0 = NoCPUReg,
 1960                         const CPURegister& arg1 = NoCPUReg,
 1961                         const CPURegister& arg2 = NoCPUReg,
 1962                         const CPURegister& arg3 = NoCPUReg);
 1963 
 1964   // Code ageing support functions.
 1965 
 1966   // Code ageing on ARM64 works similarly to on ARM. When V8 wants to mark a
 1967   // function as old, it replaces some of the function prologue (generated by
 1968   // FullCodeGenerator::Generate) with a call to a special stub (ultimately
 1969   // generated by GenerateMakeCodeYoungAgainCommon). The stub restores the
 1970   // function prologue to its initial young state (indicating that it has been
 1971   // recently run) and continues. A young function is therefore one which has a
 1972   // normal frame setup sequence, and an old function has a code age sequence
 1973   // which calls a code ageing stub.
 1974 
 1975   // Set up a basic stack frame for young code (or code exempt from ageing) with
 1976   // type FUNCTION. It may be patched later for code ageing support. This is
 1977   // done by to Code::PatchPlatformCodeAge and EmitCodeAgeSequence.
 1978   //
 1979   // This function takes an Assembler so it can be called from either a
 1980   // MacroAssembler or a PatchingAssembler context.
 1981   static void EmitFrameSetupForCodeAgePatching(Assembler* assm);
 1982 
 1983   // Call EmitFrameSetupForCodeAgePatching from a MacroAssembler context.
 1984   void EmitFrameSetupForCodeAgePatching();
 1985 
 1986   // Emit a code age sequence that calls the relevant code age stub. The code
 1987   // generated by this sequence is expected to replace the code generated by
 1988   // EmitFrameSetupForCodeAgePatching, and represents an old function.
 1989   //
 1990   // If stub is NULL, this function generates the code age sequence but omits
 1991   // the stub address that is normally embedded in the instruction stream. This
 1992   // can be used by debug code to verify code age sequences.
 1993   static void EmitCodeAgeSequence(Assembler* assm, Code* stub);
 1994 
 1995   // Call EmitCodeAgeSequence from a MacroAssembler context.
 1996   void EmitCodeAgeSequence(Code* stub);
 1997 
 1998   // Return true if the sequence is a young sequence geneated by
 1999   // EmitFrameSetupForCodeAgePatching. Otherwise, this method asserts that the
 2000   // sequence is a code age sequence (emitted by EmitCodeAgeSequence).
 2001   static bool IsYoungSequence(byte* sequence);
 2002 
 2003 #ifdef DEBUG
 2004   // Return true if the sequence is a code age sequence generated by
 2005   // EmitCodeAgeSequence.
 2006   static bool IsCodeAgeSequence(byte* sequence);
 2007 #endif
 2008 
 2009   // Jumps to found label if a prototype map has dictionary elements.
 2010   void JumpIfDictionaryInPrototypeChain(Register object, Register scratch0,
 2011                                         Register scratch1, Label* found);
 2012 
 2013  private:
 2014   // Helpers for CopyFields.
 2015   // These each implement CopyFields in a different way.
 2016   void CopyFieldsLoopPairsHelper(Register dst, Register src, unsigned count,
 2017                                  Register scratch1, Register scratch2,
 2018                                  Register scratch3, Register scratch4,
 2019                                  Register scratch5);
 2020   void CopyFieldsUnrolledPairsHelper(Register dst, Register src, unsigned count,
 2021                                      Register scratch1, Register scratch2,
 2022                                      Register scratch3, Register scratch4);
 2023   void CopyFieldsUnrolledHelper(Register dst, Register src, unsigned count,
 2024                                 Register scratch1, Register scratch2,
 2025                                 Register scratch3);
 2026 
 2027   // The actual Push and Pop implementations. These don't generate any code
 2028   // other than that required for the push or pop. This allows
 2029   // (Push|Pop)CPURegList to bundle together run-time assertions for a large
 2030   // block of registers.
 2031   //
 2032   // Note that size is per register, and is specified in bytes.
 2033   void PushHelper(int count, int size,
 2034                   const CPURegister& src0, const CPURegister& src1,
 2035                   const CPURegister& src2, const CPURegister& src3);
 2036   void PopHelper(int count, int size,
 2037                  const CPURegister& dst0, const CPURegister& dst1,
 2038                  const CPURegister& dst2, const CPURegister& dst3);
 2039 
 2040   // Perform necessary maintenance operations before a push or pop.
 2041   //
 2042   // Note that size is specified in bytes.
 2043   void PrepareForPush(Operand total_size);
 2044   void PrepareForPop(Operand total_size);
 2045 
 2046   void PrepareForPush(int count, int size) { PrepareForPush(count * size); }
 2047   void PrepareForPop(int count, int size) { PrepareForPop(count * size); }
 2048 
 2049   // Call Printf. On a native build, a simple call will be generated, but if the
 2050   // simulator is being used then a suitable pseudo-instruction is used. The
 2051   // arguments and stack (csp) must be prepared by the caller as for a normal
 2052   // AAPCS64 call to 'printf'.
 2053   //
 2054   // The 'type' argument specifies the type of the optional arguments.
 2055   void CallPrintf(CPURegister::RegisterType type = CPURegister::kNoRegister);
 2056 
 2057   // Helper for throwing exceptions.  Compute a handler address and jump to
 2058   // it.  See the implementation for register usage.
 2059   void JumpToHandlerEntry(Register exception,
 2060                           Register object,
 2061                           Register state,
 2062                           Register scratch1,
 2063                           Register scratch2);
 2064 
 2065   // Helper for implementing JumpIfNotInNewSpace and JumpIfInNewSpace.
 2066   void InNewSpace(Register object,
 2067                   Condition cond,  // eq for new space, ne otherwise.
 2068                   Label* branch);
 2069 
 2070   // Try to convert a double to an int so that integer fast-paths may be
 2071   // used. Not every valid integer value is guaranteed to be caught.
 2072   // It supports both 32-bit and 64-bit integers depending whether 'as_int'
 2073   // is a W or X register.
 2074   //
 2075   // This does not distinguish between +0 and -0, so if this distinction is
 2076   // important it must be checked separately.
 2077   //
 2078   // On output the Z flag is set if the conversion was successful.
 2079   void TryConvertDoubleToInt(Register as_int,
 2080                              FPRegister value,
 2081                              FPRegister scratch_d,
 2082                              Label* on_successful_conversion = NULL,
 2083                              Label* on_failed_conversion = NULL);
 2084 
 2085   bool generating_stub_;
 2086 #if DEBUG
 2087   // Tell whether any of the macro instruction can be used. When false the
 2088   // MacroAssembler will assert if a method which can emit a variable number
 2089   // of instructions is called.
 2090   bool allow_macro_instructions_;
 2091 #endif
 2092   bool has_frame_;
 2093 
 2094   // The Abort method should call a V8 runtime function, but the CallRuntime
 2095   // mechanism depends on CEntryStub. If use_real_aborts is false, Abort will
 2096   // use a simpler abort mechanism that doesn't depend on CEntryStub.
 2097   //
 2098   // The purpose of this is to allow Aborts to be compiled whilst CEntryStub is
 2099   // being generated.
 2100   bool use_real_aborts_;
 2101 
 2102   // This handle will be patched with the code object on installation.
 2103   Handle<Object> code_object_;
 2104 
 2105   // The register to use as a stack pointer for stack operations.
 2106   Register sp_;
 2107 
 2108   // Scratch registers available for use by the MacroAssembler.
 2109   CPURegList tmp_list_;
 2110   CPURegList fptmp_list_;
 2111 
 2112   void InitializeNewString(Register string,
 2113                            Register length,
 2114                            Heap::RootListIndex map_index,
 2115                            Register scratch1,
 2116                            Register scratch2);
 2117 
 2118  public:
 2119   // Far branches resolving.
 2120   //
 2121   // The various classes of branch instructions with immediate offsets have
 2122   // different ranges. While the Assembler will fail to assemble a branch
 2123   // exceeding its range, the MacroAssembler offers a mechanism to resolve
 2124   // branches to too distant targets, either by tweaking the generated code to
 2125   // use branch instructions with wider ranges or generating veneers.
 2126   //
 2127   // Currently branches to distant targets are resolved using unconditional
 2128   // branch isntructions with a range of +-128MB. If that becomes too little
 2129   // (!), the mechanism can be extended to generate special veneers for really
 2130   // far targets.
 2131 
 2132   // Helps resolve branching to labels potentially out of range.
 2133   // If the label is not bound, it registers the information necessary to later
 2134   // be able to emit a veneer for this branch if necessary.
 2135   // If the label is bound, it returns true if the label (or the previous link
 2136   // in the label chain) is out of range. In that case the caller is responsible
 2137   // for generating appropriate code.
 2138   // Otherwise it returns false.
 2139   // This function also checks wether veneers need to be emitted.
 2140   bool NeedExtraInstructionsOrRegisterBranch(Label *label,
 2141                                              ImmBranchType branch_type);
 2142 };
 2143 
 2144 
 2145 // Use this scope when you need a one-to-one mapping bewteen methods and
 2146 // instructions. This scope prevents the MacroAssembler from being called and
 2147 // literal pools from being emitted. It also asserts the number of instructions
 2148 // emitted is what you specified when creating the scope.
 2149 class InstructionAccurateScope BASE_EMBEDDED {
 2150  public:
 2151   InstructionAccurateScope(MacroAssembler* masm, size_t count = 0)
 2152       : masm_(masm)
 2153 #ifdef DEBUG
 2154         ,
 2155         size_(count * kInstructionSize)
 2156 #endif
 2157   {
 2158     // Before blocking the const pool, see if it needs to be emitted.
 2159     masm_->CheckConstPool(false, true);
 2160     masm_->CheckVeneerPool(false, true);
 2161 
 2162     masm_->StartBlockPools();
 2163 #ifdef DEBUG
 2164     if (count != 0) {
 2165       masm_->bind(&start_);
 2166     }
 2167     previous_allow_macro_instructions_ = masm_->allow_macro_instructions();
 2168     masm_->set_allow_macro_instructions(false);
 2169 #endif
 2170   }
 2171 
 2172   ~InstructionAccurateScope() {
 2173     masm_->EndBlockPools();
 2174 #ifdef DEBUG
 2175     if (start_.is_bound()) {
 2176       ASSERT(masm_->SizeOfCodeGeneratedSince(&start_) == size_);
 2177     }
 2178     masm_->set_allow_macro_instructions(previous_allow_macro_instructions_);
 2179 #endif
 2180   }
 2181 
 2182  private:
 2183   MacroAssembler* masm_;
 2184 #ifdef DEBUG
 2185   size_t size_;
 2186   Label start_;
 2187   bool previous_allow_macro_instructions_;
 2188 #endif
 2189 };
 2190 
 2191 
 2192 // This scope utility allows scratch registers to be managed safely. The
 2193 // MacroAssembler's TmpList() (and FPTmpList()) is used as a pool of scratch
 2194 // registers. These registers can be allocated on demand, and will be returned
 2195 // at the end of the scope.
 2196 //
 2197 // When the scope ends, the MacroAssembler's lists will be restored to their
 2198 // original state, even if the lists were modified by some other means.
 2199 class UseScratchRegisterScope {
 2200  public:
 2201   explicit UseScratchRegisterScope(MacroAssembler* masm)
 2202       : available_(masm->TmpList()),
 2203         availablefp_(masm->FPTmpList()),
 2204         old_available_(available_->list()),
 2205         old_availablefp_(availablefp_->list()) {
 2206     ASSERT(available_->type() == CPURegister::kRegister);
 2207     ASSERT(availablefp_->type() == CPURegister::kFPRegister);
 2208   }
 2209 
 2210   ~UseScratchRegisterScope();
 2211 
 2212   // Take a register from the appropriate temps list. It will be returned
 2213   // automatically when the scope ends.
 2214   Register AcquireW() { return AcquireNextAvailable(available_).W(); }
 2215   Register AcquireX() { return AcquireNextAvailable(available_).X(); }
 2216   FPRegister AcquireS() { return AcquireNextAvailable(availablefp_).S(); }
 2217   FPRegister AcquireD() { return AcquireNextAvailable(availablefp_).D(); }
 2218 
 2219   Register UnsafeAcquire(const Register& reg) {
 2220     return Register(UnsafeAcquire(available_, reg));
 2221   }
 2222 
 2223   Register AcquireSameSizeAs(const Register& reg);
 2224   FPRegister AcquireSameSizeAs(const FPRegister& reg);
 2225 
 2226  private:
 2227   static CPURegister AcquireNextAvailable(CPURegList* available);
 2228   static CPURegister UnsafeAcquire(CPURegList* available,
 2229                                    const CPURegister& reg);
 2230 
 2231   // Available scratch registers.
 2232   CPURegList* available_;     // kRegister
 2233   CPURegList* availablefp_;   // kFPRegister
 2234 
 2235   // The state of the available lists at the start of this scope.
 2236   RegList old_available_;     // kRegister
 2237   RegList old_availablefp_;   // kFPRegister
 2238 };
 2239 
 2240 
 2241 inline MemOperand ContextMemOperand(Register context, int index) {
 2242   return MemOperand(context, Context::SlotOffset(index));
 2243 }
 2244 
 2245 inline MemOperand GlobalObjectMemOperand() {
 2246   return ContextMemOperand(cp, Context::GLOBAL_OBJECT_INDEX);
 2247 }
 2248 
 2249 
 2250 // Encode and decode information about patchable inline SMI checks.
 2251 class InlineSmiCheckInfo {
 2252  public:
 2253   explicit InlineSmiCheckInfo(Address info);
 2254 
 2255   bool HasSmiCheck() const {
 2256     return smi_check_ != NULL;
 2257   }
 2258 
 2259   const Register& SmiRegister() const {
 2260     return reg_;
 2261   }
 2262 
 2263   Instruction* SmiCheck() const {
 2264     return smi_check_;
 2265   }
 2266 
 2267   // Use MacroAssembler::InlineData to emit information about patchable inline
 2268   // SMI checks. The caller may specify 'reg' as NoReg and an unbound 'site' to
 2269   // indicate that there is no inline SMI check. Note that 'reg' cannot be csp.
 2270   //
 2271   // The generated patch information can be read using the InlineSMICheckInfo
 2272   // class.
 2273   static void Emit(MacroAssembler* masm, const Register& reg,
 2274                    const Label* smi_check);
 2275 
 2276   // Emit information to indicate that there is no inline SMI check.
 2277   static void EmitNotInlined(MacroAssembler* masm) {
 2278     Label unbound;
 2279     Emit(masm, NoReg, &unbound);
 2280   }
 2281 
 2282  private:
 2283   Register reg_;
 2284   Instruction* smi_check_;
 2285 
 2286   // Fields in the data encoded by InlineData.
 2287 
 2288   // A width of 5 (Rd_width) for the SMI register preclues the use of csp,
 2289   // since kSPRegInternalCode is 63. However, csp should never hold a SMI or be
 2290   // used in a patchable check. The Emit() method checks this.
 2291   //
 2292   // Note that the total size of the fields is restricted by the underlying
 2293   // storage size handled by the BitField class, which is a uint32_t.
 2294   class RegisterBits : public BitField<unsigned, 0, 5> {};
 2295   class DeltaBits : public BitField<uint32_t, 5, 32-5> {};
 2296 };
 2297 
 2298 } }  // namespace v8::internal
 2299 
 2300 #ifdef GENERATED_CODE_COVERAGE
 2301 #error "Unsupported option"
 2302 #define CODE_COVERAGE_STRINGIFY(x) #x
 2303 #define CODE_COVERAGE_TOSTRING(x) CODE_COVERAGE_STRINGIFY(x)
 2304 #define __FILE_LINE__ __FILE__ ":" CODE_COVERAGE_TOSTRING(__LINE__)
 2305 #define ACCESS_MASM(masm) masm->stop(__FILE_LINE__); masm->
 2306 #else
 2307 #define ACCESS_MASM(masm) masm->
 2308 #endif
 2309 
 2310 #endif  // V8_ARM64_MACRO_ASSEMBLER_ARM64_H_

