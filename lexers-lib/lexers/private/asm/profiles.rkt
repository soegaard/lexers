#lang racket/base

;;;
;;; Assembly Profile Rules
;;;

;; Compact lexical rules for the implemented assembly language profiles.  The
;; rules intentionally identify syntax, not a particular assembler executable.

;; asm-dialect-spec?        -- Recognize one private lexical profile rule set.
;; asm-profile-spec          -- Resolve rules for a dialect and architecture.
;; asm-profile-architectures -- List the target architectures accepted by a profile.
;; asm-word-register?        -- Recognize a register spelling in a profile.
;; asm-word-instruction?     -- Recognize a small real instruction subset.
;; asm-word-directive?       -- Recognize a profile directive or macro word.

(provide (struct-out asm-dialect-spec)
         asm-profile-spec
         asm-profile-architectures
         asm-word-register?
         asm-word-instruction?
         asm-word-directive?
         asm-word-macro-open?
         asm-word-macro-close?)

(require racket/list
         racket/string)

;; The scanner's rules.  `line-comments` contains single-character markers;
;; `c-comments?` enables // and /* ... */.  A semicolon can instead remain a
;; statement delimiter, which matters for GNU targets and Kick script blocks.
(struct asm-dialect-spec (dialect architecture line-comments c-comments?
                                  semicolon-delimiter? directives macro-open
                                  macro-close instructions register-kind
                                  word-prefixes)
  #:transparent)

;; Small, real instruction subsets.  Unknown mnemonics deliberately remain
;; valid identifiers: lexer coloring must not turn future instructions or macro
;; names into errors.
(define mos6502-instructions
  '(adc and asl bcc bcs beq bit bmi bne bpl brk bvc bvs clc cld cli clv cmp
        cpx cpy dec dex dey eor inc inx iny jmp jsr lda ldx ldy lsr nop ora
        pha php pla plp rol ror rti rts sbc sec sed sei sta stx sty tax tay
        tsx txa txs tya))

(define m68k-instructions
  '(abcd add adda addi addq addx and andi asl asr bcc bcs beq bge bgt bhi ble
        blt bmi bne bpl bra bsr bvc bvs chk clr cmp cmpa cmpi cmpm dbra dbcc
        divs divu eor exg ext jmp jsr lea link lsl lsr move movea movem movep
        moveq muls mulu nbcd neg negx nop not or ori pea reset rol ror roxl
        roxr rte rtr rts sbcd scc stop sub suba subi subq subx swap tas trap
        tst unlk))

(define x86-instructions
  '(adc add and call cdq cmp dec div idiv imul inc int jmp lea leave mov movzx
        movsx mul neg nop not or pop push ret rol ror sal sar sbb setz shl shr
        sub test xchg xor je jne jz jnz ja jb jg jl jge jle jo jno js jns))

(define arm-instructions
  '(adc add adr and asr b bcc bcs beq bge bgt bhi bhs ble bls blt bmi bne bpl
        bvc bvs bic bl blx bx cmp eor ldm ldr ldrb ldrh ldrsb ldrsh lsl lsr
        mov movt movw mul mvn orr pop push ror rsb sbc stm str strb strh sub
        svc teq tst))

(define aarch64-instructions
  '(adc add adds adrp adr and ands b bl blr br ccmp cmp csel csinc cset eor
        fadd fcmp fmov ldp ldr ldrb ldrh ldrsw lsl lsr madd mov movk movz mul
        orr ret sbc stp str strb strh sub subs tbz tbnz ubfx))

;; Directive sets cover the documented syntax markers and common structural
;; forms.  They are lexical evidence only; the lexer never executes them.
(define ca65-directives
  '(.a16 .a8 .addr .align .asciiz .bss .byte .code .data .define .dword .else
        .elseif .end .endmacro .endproc .endscope .endif .export .exportzp
        .feature .if .ifdef .ifndef .import .importzp .include .incbin .local
        .localchar .macro .org .p02 .p816 .proc .rodata .scope .segment .setcpu
        .word .zeropage))

(define acme-directives
  '(!8 !16 !binary !byte !cpu !endoffile !fill !for !if !ifdef !ifndef !macro
       !pseudopc !source !text !to !word !zone))

(define motorola-directives
  '(bss cnop dc.b dc.l dc.w dcb.b dcb.l dcb.w ds.b ds.l ds.w end endm equ equr
        even idnt include incbin macro near obj section text xdef xref))

(define kick-directives
  '(|#import| |#importonce| .break .const .disk .else .endif .for .function .if .import
        .label .macro .pc .print .pseudocommand .segment .segmentdef .var
        .while))

(define tass-directives
  '(.align .bend .block .break .byte .cpu .cerror .dsection .else .elsif .enc
        .endblock .endlogical .endmacro .endif .fill .function .if .logical
        .macro .namespace .endnamespace .section .text .word))

(define gas-directives
  '(.align .arm .att_syntax .bss .byte .code16 .code16gcc .code32 .code64 .data
        .else .elseif .endm .endif .equ .globl .global .if .include .intel_syntax
        .macro .section .set .size .syntax .text .thumb .thumb_func .word .long
        .quad .type))

(define nasm-directives
  '(absolute bits common cpu default db dd dq dt dw dy dz equ extern global incbin
        include org resb resd resq resw section segment struc endstruc times
        %assign %define %elif %else %endif %if %ifdef %ifndef %imacro %include
        %macro %undef %xdefine %xmacro %endmacro))

;; normalize-word : string? -> symbol?
;;   Compare language words case-insensitively while preserving source text.
(define (normalize-word text)
  (string->symbol (string-downcase text)))

;; make-spec : symbol? symbol? ... -> asm-dialect-spec?
;;   Construct one compact rule record.
(define (make-spec dialect architecture line-comments c-comments?
                   semicolon-delimiter? directives macro-open macro-close
                   instructions register-kind word-prefixes)
  (asm-dialect-spec dialect architecture line-comments c-comments?
                    semicolon-delimiter? directives macro-open macro-close
                    instructions register-kind word-prefixes))

;; asm-profile-architectures : symbol? -> (listof symbol?)
;;   List the architectures that must be explicit for a target-dependent dialect.
(define (asm-profile-architectures dialect)
  (case dialect
    [(gas) '(m68k x86 arm aarch64)]
    [else '()]))

;; asm-profile-spec : symbol? (or/c symbol? #f) -> (or/c asm-dialect-spec? #f)
;;   Resolve lexical rules without substituting a neighbouring language profile.
(define (asm-profile-spec dialect architecture)
  (case dialect
    [(generic)
     (make-spec 'generic #f '() #f #f '() '() '() '() 'none '())]
    [(ca65)
     (make-spec 'ca65 'mos-6502 '(#\;) #f #f ca65-directives '(.macro) '(.endmacro)
                mos6502-instructions 'none '(#\@))]
    [(acme)
     (make-spec 'acme 'mos-6502 '(#\;) #f #f acme-directives '(!macro) '(!endmacro)
                mos6502-instructions 'none '(#\! #\+))]
    [(native-motorola)
     (make-spec 'native-motorola 'm68k '(#\;) #f #f motorola-directives '(macro) '(endm)
                m68k-instructions 'm68k '())]
    [(asmone)
     (make-spec 'asmone 'm68k '(#\;) #f #f motorola-directives '(macro) '(endm)
                m68k-instructions 'm68k '())]
    [(devpac)
     (make-spec 'devpac 'm68k '(#\;) #f #f motorola-directives '(macro) '(endm)
                m68k-instructions 'm68k '())]
    [(kick)
     (make-spec 'kick 'mos-6502 '() #t #t kick-directives '(.macro) '(.endmacro)
                mos6502-instructions 'none '(#\! #\#))]
    [(|64tass|)
     (make-spec '|64tass| 'mos-6502 '(#\;) #f #f tass-directives '(.macro) '(.endmacro)
                mos6502-instructions 'none '())]
    [(nasm)
     (make-spec 'nasm 'x86 '(#\;) #f #f nasm-directives '(%macro %imacro) '(%endmacro)
                x86-instructions 'x86 '(#\%))]
    [(gas)
     (case architecture
       [(m68k)
        (make-spec 'gas 'm68k '(#\|) #t #t gas-directives '(.macro) '(.endm)
                   m68k-instructions 'm68k '(#\%))]
       [(x86)
        (make-spec 'gas 'x86 '(#\#) #t #t gas-directives '(.macro) '(.endm)
                   x86-instructions 'x86 '(#\%))]
       [(arm)
        (make-spec 'gas 'arm '(#\@) #t #t gas-directives '(.macro) '(.endm)
                   arm-instructions 'arm '())]
       [(aarch64)
        (make-spec 'gas 'aarch64 '() #t #t gas-directives '(.macro) '(.endm)
                   aarch64-instructions 'aarch64 '())]
       [else #f])]
    [else #f]))

;; asm-word-register? : asm-dialect-spec? string? -> boolean?
;;   Recognize a practical register subset for one selected architecture.
(define (asm-word-register? spec text)
  (define normalized
    (string-downcase (if (and (positive? (string-length text))
                              (char=? (string-ref text 0) #\%))
                         (substring text 1)
                         text)))
  (case (asm-dialect-spec-register-kind spec)
    [(m68k)
     (or (member normalized '("sp" "fp" "pc" "zpc"))
         (regexp-match? #px"^[ad][0-7]$" normalized))]
    [(x86)
     (or (member normalized '("al" "ah" "bl" "bh" "cl" "ch" "dl" "dh"
                              "ax" "bx" "cx" "dx" "si" "di" "sp" "bp"
                              "eax" "ebx" "ecx" "edx" "esi" "edi" "esp" "ebp"
                              "rax" "rbx" "rcx" "rdx" "rsi" "rdi" "rsp" "rbp"
                              "rip" "cs" "ds" "es" "fs" "gs" "ss"))
         (regexp-match? #px"^r(?:[89]|1[0-5])(?:b|w|d)?$" normalized))]
    [(arm)
     (or (member normalized '("sp" "lr" "pc" "ip" "fp"))
         (regexp-match? #px"^r(?:[0-9]|1[0-5])$" normalized))]
    [(aarch64)
     (or (member normalized '("sp" "wsp" "xzr" "wzr" "lr" "fp"))
         (regexp-match? #px"^[xw](?:[0-9]|[12][0-9]|30)$" normalized)
         (regexp-match? #px"^v(?:[0-9]|[12][0-9]|3[01])(?:\\.(?:[0-9]|1[0-9]|2[0-9]|3[0-1])[bhsd])?$"
                        normalized))]
    [else #f]))

;; asm-word-instruction? : asm-dialect-spec? string? -> boolean?
;;   Recognize documented instruction spelling, including m68k and x86 suffixes.
(define (asm-word-instruction? spec text)
  (define normalized
    (normalize-word text))
  (or (member normalized (asm-dialect-spec-instructions spec))
      (and (memq (asm-dialect-spec-architecture spec) '(m68k x86))
           (regexp-match? #px"^[a-z][a-z0-9]*\\.[bwlqsx]$"
                          (symbol->string normalized)))))

;; asm-word-directive? : asm-dialect-spec? string? -> boolean?
;;   Recognize one structural directive without assigning executable identity.
(define (asm-word-directive? spec text)
  (member (normalize-word text)
          (asm-dialect-spec-directives spec)))

;; asm-word-macro-open? : asm-dialect-spec? string? -> boolean?
;;   Recognize the beginning of one profile macro definition.
(define (asm-word-macro-open? spec text)
  (member (normalize-word text)
          (asm-dialect-spec-macro-open spec)))

;; asm-word-macro-close? : asm-dialect-spec? string? -> boolean?
;;   Recognize the end of one profile macro definition.
(define (asm-word-macro-close? spec text)
  (member (normalize-word text)
          (asm-dialect-spec-macro-close spec)))
