
namespace templating {

    
    enum class Register_e : Opcodes::insn_size_t {
        r0 =0,
        r1,
        r2,
        r3,
        r4,
        r5,
        r6,
        r7,
        r8,
        r9,
        r10,
        r11,
        r12,
        sp,
        lr,
        pc
    };
    using Opcodes::high_flag_e;
    using Opcodes::insn_t;


    /*
        not a valid thumb instruction
    */
    static constexpr insn_t ABSTRACT_INSN = 0xEAFF;
    constexpr bool isLoreg(Register_e reg) {
        return acceptEnumClass(reg) < acceptEnumClass(Register_e::r7);
    }

    constexpr bool isHireg(Register_e reg) {
        return !isLoreg(reg);
    }

    constexpr bool eitherHigh(Register_e reg1, Register_e reg2) {
        return isHireg(reg1) || isHireg(reg2);
    }

    constexpr high_flag_e chooseHighFlag(Register_e reg) {
        return isLoreg(reg) ? high_flag_e::low : high_flag_e::high;
    }
    constexpr Opcodes::anyreg_t makeAnyreg(Register_e reg) {
        return isHireg(reg) ? acceptEnumClass(reg) - acceptEnumClass(Register_e::r8) : acceptEnumClass(reg);
    }

    constexpr Opcodes::loreg_t makeLoreg(Register_e reg) {
        return static_cast<Opcodes::loreg_t>(acceptEnumClass(reg));
    }

    constexpr insn_t highOpAdapter(Opcodes::high_op_e op, Register_e destination, Register_e source) {
            return Opcodes::high_op(op, 
           chooseHighFlag(destination), chooseHighFlag(source), makeAnyreg(source), makeAnyreg(destination));
    }

    constexpr insn_t mov(Register_e destination, Register_e source) {
        if(eitherHigh(destination, source)) {
            return highOpAdapter(Opcodes::high_op_e::MOV, destination, source);  
        }
        else {
            return Opcodes::move_shifted_reg(Opcodes::shift_type_e::LSL, 0, 
            makeLoreg(source), 
            makeLoreg(destination));
        }
    }

    constexpr insn_t add(Register_e destination, Register_e source) {
        if(eitherHigh(destination, source)) {
            return highOpAdapter(Opcodes::high_op_e::ADD, destination, source);  
        }
        else {
            return Opcodes::add_subtract(Opcodes::imm_or_reg_e::REG, Opcodes::add_or_sub_e::ADD, 
            makeLoreg(source), makeLoreg(source), 
            makeLoreg(destination));
        }
    }

    constexpr insn_t aluAdapter(Opcodes::alu_op_e op, Register_e destination, Register_e source ) {
        CE_ASSERT(!eitherHigh(destination, source), "aluAdapter does not work with high regs.");
        return Opcodes::alu_reg2reg_op(op,  
            makeLoreg(source), makeLoreg(destination));
    }

    constexpr insn_t cmp(Register_e destination, Register_e source) {
        if(eitherHigh(destination, source)) {
            return highOpAdapter(Opcodes::high_op_e::CMP, destination, source);  
        }
        else {
            return aluAdapter(Opcodes::alu_op_e::CMP, destination, source);
        }
    }

    #define ADAPT_ALU(funcname, opcode)   \
    constexpr insn_t funcname (Register_e destination, Register_e source) {\
        CE_ASSERT(!eitherHigh(destination, source), "Cannot " #funcname " a high reg."); \
        return aluAdapter(Opcodes::alu_op_e::opcode, destination, source);\
    }
    ADAPT_ALU(_and, AND)
    ADAPT_ALU(eor, EOR)
    ADAPT_ALU(lsl, LSL)
    ADAPT_ALU(lsr, LSR)
    ADAPT_ALU(asr, ASR)
    ADAPT_ALU(adc, ADC)
    ADAPT_ALU(sbc, SBC)
    ADAPT_ALU(ror, ROR)
    ADAPT_ALU(tst, TST)
    ADAPT_ALU(neg, NEG)
    ADAPT_ALU(cmn, CMN)
    ADAPT_ALU(orr, ORR)
    ADAPT_ALU(mul, MUL)
    ADAPT_ALU(bic, BIC)
    ADAPT_ALU(mvn, MVN)

    #undef ADAPT_ALU
    constexpr bool isAddrReg(Register_e reg) {
        return reg == Register_e::sp || reg == Register_e::pc;
    }
    constexpr insn_t add(Register_e destination, Register_e source, int imm) {
        bool sign = imm < 0;
        int absolute = sign ? -imm : imm;
        bool destIsSrc = destination == source;
        if(!eitherHigh(destination, source) && destIsSrc && !sign && imm < 256) {
            return Opcodes::mov_cmp_add_sub_imm(Opcodes::imm_ops_e::ADD, makeLoreg(source), imm);
        }
        else if(destination == Register_e::sp && destIsSrc) {
            CE_ASSERT(absolute % 4 == 0, "value added to sp must be word aligned");
            return Opcodes::offset_sp_swordoff7(sign ? Opcodes::offs_sign_e::neg : Opcodes::offs_sign_e::pos,
                absolute >> 2);
        }
        else if(!destIsSrc && absolute < 8 && !eitherHigh(destination, source) ) {
            return Opcodes::add_subtract(Opcodes::imm_or_reg_e::IMM, 
            sign ? Opcodes::add_or_sub_e::SUB : Opcodes::add_or_sub_e::ADD, absolute, 
            makeLoreg(source), makeLoreg(destination));
        }
        else if(isAddrReg(source) && !destIsSrc && !sign && !(imm % 4)) {
            int encodedValue = imm >> 2;
            CE_ASSERT(encodedValue < 256, "Value is too large to add directly to an addr reg.");
            CE_ASSERT(isLoreg(destination), "Can't store addr result in hireg");
            return Opcodes::loadaddr_basereg_wordoff8(source == Register_e::sp 
            ? Opcodes::addr_reg_e::SP 
            : Opcodes::addr_reg_e::PC, makeLoreg(destination), encodedValue);

        }
        else {
            CE_ASSERT(false, "Couldn't encode add imm!");
        }
    }





    constexpr insn_t bx(Register_e to) {
        return highOpAdapter(Opcodes::high_op_e::BX, Register_e::r0, to);  
    }

    struct KnowableState {
        unsigned char* priorbyte;
        uword_t npriorbytes;
        uword_t r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, sp, lr, pc;
        uword_t known_mask;

        bool is_known(Register_e reg) {
            return (known_mask & (1 << acceptEnumClass(reg))) != 0;
        }
    };



    using AbstractResolver_t = insn_t (*)(KnowableState*);

    class AbstractMarker {
        uword_t m_code_index;
        AbstractResolver_t m_resolver;
    public:
        constexpr AbstractMarker() : m_code_index(0), m_resolver(nullptr) {}
        constexpr AbstractMarker(uword_t code_index, AbstractResolver_t resolver) 
        : m_code_index(code_index),
        m_resolver(resolver) {}

        inline insn_t operator ()(KnowableState* state) {
            return m_resolver(state);
        }

        inline bool operator == (uword_t idx) {
            return idx == m_code_index;
        }

    };



    constexpr uword_t MAX_TEMPLATE_INSNS = 256;
    constexpr uword_t MAX_ABSTRACT_MARKERS = 32;
    class InsnStream {
        insn_t code[MAX_TEMPLATE_INSNS];
        uword_t position;
        AbstractMarker markers[MAX_ABSTRACT_MARKERS];
        uword_t currentAbstractCount;

        bool waiting_for_abstract_handler;
        uword_t last_abstract_pos;
    public:
        constexpr InsnStream() : code(), position(0), markers(), currentAbstractCount(0),
        waiting_for_abstract_handler(false),
        last_abstract_pos(-1){}



        constexpr InsnStream& operator <<(insn_t insn) {
            CE_ASSERT(!waiting_for_abstract_handler, "Previous generated instruction was abstract, but no handler was ever provided");
            CE_ASSERT(position < MAX_TEMPLATE_INSNS, "Runaway generation of template, max instructions exceeded.");
            code[position++] = insn;
            if(insn == ABSTRACT_INSN) {
                waiting_for_abstract_handler = true;
                last_abstract_pos = position-1;
            }
            return *this;
        }

        constexpr InsnStream& operator << (AbstractResolver_t resolver) {
            /*
                handler is being provided for previous instruction
                that may have generated an abstract instruction
                add the marker if it was abstract, else do nothing
            */
            if(waiting_for_abstract_handler) {
                CE_ASSERT(currentAbstractCount < MAX_ABSTRACT_MARKERS, "Too many abstract instruction were generated!");
                waiting_for_abstract_handler = false;
                markers[currentAbstractCount++] = {last_abstract_pos, resolver};
            }
            return *this;
        }

        constexpr uword_t size() const {
            return position;
        }
        constexpr uword_t abstractCount() const {
            return currentAbstractCount;
        }

        constexpr insn_t insnAt(uword_t index) const {
            return code[index];
        }
        constexpr AbstractMarker markerAt(uword_t index) {
            return markers[index];
        }

    };

    template<auto& finalStream>
    struct RTTemplate {
        static constexpr uword_t template_code_size = finalStream.size();
        static constexpr uword_t template_abstract_count = finalStream.abstractCount();
        insn_t code[template_code_size];
        AbstractMarker markers[template_abstract_count];

        constexpr RTTemplate() : code(), markers() {
            for(uword_t i = 0; i < template_code_size; ++i) {
                code[i] = finalStream.insnAt(i);
            }
            for(uword_t i = 0; i < template_abstract_count; ++i) {
                markers[i] = finalStream.markerAt(i);
            }

        }

        template<uword_t current = 0>
        inline insn_t dorepl(KnowableState* state, insn_t in_insn, uword_t idx) {
            if(idx == markers[current]) {
                return markers[current](state);
            }
            else {
                if constexpr(current+1 < template_abstract_count) {
                    return dorepl<current+1>(state, in_insn, idx);
                }
                else {
                    return in_insn;
                }
            }
        }

        void emit(void* dataDestination) {
            uword_t pc = reinterpret_cast<uword_t>(dataDestination);
            KnowableState state{0};

            for(uword_t i = 0; i < template_code_size; ++i, pc += 2) {
                insn_t currentInsn = code[i];
                if constexpr(template_abstract_count != 0) {
                    state.pc = pc + 4;
                    currentInsn = dorepl(state, currentInsn, i);
                }
                reinterpret_cast<insn_t*>(dataDestination)[i] = currentInsn;
            }

        }
    };

    constexpr InsnStream hmmm() {
        InsnStream result;
        result << add(Register_e::r0, Register_e::r1);
        result << bx(Register_e::lr);
        return result;
    }

    template<InsnStream (*func)()>
    constexpr auto produceTemplate() {
        struct ResultHelper {
            static constexpr InsnStream funcResult = func();
            static constexpr RTTemplate<funcResult> result {};
        };
        return ResultHelper::result;
    }

    constexpr auto emitter = produceTemplate<hmmm>();


    constexpr insn_t TEST = add(Register_e::r5, Register_e::pc, 48);

}