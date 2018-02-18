using uword_t = unsigned;
template<typename T>
constexpr auto acceptEnumClass(T x) {
    if constexpr(__is_enum(T))
        return static_cast<__underlying_type(T)>(x);
    else
        return x;
}

template<typename T>
constexpr uword_t bitSizeof = sizeof(T) * 8;
namespace Opcodes {

    using insn_size_t = unsigned short;
    using insn_t = unsigned short;


    class ThumbInsnBitstream {
        insn_t m_value;
        uword_t m_pos;
    public:

        class ThumbStreamInsertion {
            ThumbInsnBitstream& m_bstream;
        public:
            constexpr ThumbStreamInsertion(ThumbInsnBitstream* bstream) : m_bstream(*bstream) {}

            constexpr ThumbInsnBitstream& operator << (uword_t value) {
                m_bstream.m_pos += value;
                return m_bstream;
            }
        };

        constexpr ThumbInsnBitstream() : m_value(0), m_pos(0) {}

        template<typename T>
        constexpr ThumbStreamInsertion operator << (T value) {
            m_value |= (acceptEnumClass(value) << m_pos);
            return (ThumbStreamInsertion){this};
        }

        constexpr insn_t finish() {
            return __builtin_bswap16(m_value);
        }

    };
    #define OPSTART ThumbInsnBitstream bits

    #define OPEND return bits.finish()

    enum class shift_type_e : insn_size_t {
        LSL =  0,
        LSR = 1,
        ASR = 2
    };

    enum class add_or_sub_e : insn_size_t {
        ADD = 0,
        SUB = 1
    };

    enum class imm_or_reg_e : insn_size_t {
        REG = 0,
        IMM = 1
    };

    enum class imm_ops_e : insn_size_t {
        MOV, 
        CMP,
        ADD,
        SUB
    };

    enum class alu_op_e : insn_size_t {
        AND,
        EOR,
        LSL,
        LSR,
        ASR,
        ADC,
        SBC,
        ROR,
        TST,
        NEG,
        CMP,
        CMN,
        ORR,
        MUL,
        BIC,
        MVN
    };
    enum class high_op_e : insn_size_t {
        ADD,
        CMP,
        MOV,
        BX
    };

    enum class high_flag_e : insn_size_t {
        low,
        high
    };

    enum class loadstr_e : insn_size_t {
        store,
        load
    };

    enum class byte_or_word_e : insn_size_t {
        word,
        byte
    };

    enum class sign_load_op_e : insn_size_t {
        strh,
        ldsb,
        ldrh,
        
        ldsh
    };

    enum class addr_reg_e : insn_size_t {
        PC,
        SP
    };

    enum class pushflow_e : insn_size_t {
        noflow,
        ret_or_save
    };

    using reglist_t = insn_size_t;

    using rn_or_offset_t = insn_size_t;

    enum loreg_t : insn_size_t {
        LOREG_R0 = 0,
        LOREG_R1 = 1,
        LOREG_R2,
        LOREG_R3,
        LOREG_R4,
        LOREG_R5,
        LOREG_R6,
        LOREG_R7
    };

    enum class offs_sign_e : insn_size_t {
        pos,
        neg
    };

    enum class condition_e : insn_size_t {
        BEQ,
        BNE,
        BCS,
        BCC,
        BMI,
        BPL,
        BVS,
        BVC,
        BHI,
        BLS,
        BGE,
        BLT,
        BGT,
        BLE
    };

    enum class lohigh_offs_e : insn_size_t {
        high,
        low
    };

    using anyreg_t = insn_size_t;
    using largebranch_t = insn_size_t;

    namespace encoding {
        static constexpr uword_t LOREG_BITS = 3;
        static constexpr uword_t SHIFT_BITS = 5;
        static constexpr uword_t SWI_BITS = 8;
        static constexpr uword_t ALU_OP_BITS = 4;
        static constexpr uword_t ANYREG_BITS = 3;
        static constexpr uword_t HIGH_OP_BITS = 2;
        static constexpr uword_t REGLIST_BITS = 8;
        static constexpr uword_t CONDITION_BITS = 4;
        static constexpr uword_t LARGEBRANCH_BITS = 11;
    }

    constexpr
    insn_t move_shifted_reg(
        shift_type_e shift_type, 
        insn_size_t shift_value, 
        loreg_t source, 
        loreg_t dest) {
        
        OPSTART;
        bits << dest << encoding::LOREG_BITS 
         << source << encoding::LOREG_BITS
         << shift_value << encoding::SHIFT_BITS 
         << shift_type << 2;
        OPEND;
    }




    constexpr insn_t add_subtract(imm_or_reg_e imm_flag, add_or_sub_e opcode, rn_or_offset_t value, loreg_t source, loreg_t dest) {
        
        OPSTART;
        bits << dest << encoding::LOREG_BITS
         << source << encoding::LOREG_BITS
         << value << encoding::LOREG_BITS
         << opcode << 1
         << imm_flag << 1
         << 0b00011;

        OPEND;
    }

    constexpr insn_t mov_cmp_add_sub_imm(imm_ops_e op, loreg_t Rd, insn_size_t Imm8) {
        OPSTART;
        bits << Imm8 << 8 << Rd << encoding::LOREG_BITS << op << 2 << 0b001 << 3;
        OPEND;
    }

    constexpr insn_t alu_reg2reg_op(alu_op_e op, loreg_t source, loreg_t dest) {
        OPSTART;
        bits << dest << encoding::LOREG_BITS << source <<
            encoding::LOREG_BITS << op << encoding::ALU_OP_BITS << 0b010000;
        OPEND;
    }

 

    constexpr insn_t high_op(high_op_e op, 
   high_flag_e hSource, high_flag_e hDest, anyreg_t source, anyreg_t dest) {
        OPSTART;
        bits << dest << encoding::ANYREG_BITS 
        << source << encoding::ANYREG_BITS
        << hDest << 1 
        << hSource << 1
        << op << encoding::HIGH_OP_BITS
        << 0b010001;
        OPEND;
    }

   constexpr insn_t load_pcrel(loreg_t Rd, insn_size_t WordOffset) {
        OPSTART;
        bits << WordOffset << 8 << Rd << encoding::LOREG_BITS << 0b01001;
        OPEND;
    }

    constexpr insn_t loadstr_regoff(loadstr_e op, byte_or_word_e datatype, 
    loreg_t Roffset, loreg_t Rbase, loreg_t Rdest) {
        OPSTART;
        bits << Rdest << encoding::LOREG_BITS
        << Rbase << encoding::LOREG_BITS
        << Roffset << encoding::LOREG_BITS
        << 0 << 1
        << datatype << 1
        << op << 1
        << 0b0101;
        OPEND;
    }

        constexpr insn_t loadstr_sign_regoff(sign_load_op_e op,  
        loreg_t Roffset, loreg_t Rbase, loreg_t Rdest) {
        OPSTART;
        bits << Rdest << encoding::LOREG_BITS
        << Rbase << encoding::LOREG_BITS
        << Roffset << encoding::LOREG_BITS
        << 1 << 1
        << op << 2
        << 0b0101;
        OPEND;
    }

    constexpr insn_t loadstr_immoff(loadstr_e op, byte_or_word_e datatype, 
    insn_size_t offset5, loreg_t base, loreg_t dest_or_src) {
        OPSTART;
        bits << dest_or_src << encoding::LOREG_BITS
        << base << encoding::LOREG_BITS
        << offset5 << 5
        << op << 1
        << datatype << 1
        << 0b011;

        OPEND;
    }

    constexpr insn_t loadstr_halfword_immoff(loadstr_e op, insn_size_t offset5, loreg_t
    base, loreg_t dest_or_src) {
        OPSTART;
        bits << dest_or_src << encoding::LOREG_BITS
        << base << encoding::LOREG_BITS
        << offset5 << 5
        << op << 1

        << 0b1000;
        OPEND;
    }


    constexpr insn_t loadstr_sprel_wordoff8(loadstr_e op, loreg_t dest_or_src, insn_size_t word8) {
        OPSTART;
        bits << word8 << 8 
        << dest_or_src << encoding::LOREG_BITS
        << op << 1
        << 0b1001;
        OPEND;
    }
    constexpr insn_t loadaddr_basereg_wordoff8(addr_reg_e addrbase, loreg_t dest, insn_size_t word8) {
        OPSTART;
        bits << word8 << 8
        << dest << encoding::LOREG_BITS
        << addrbase << 1
        << 0b1010;
        OPEND;
    }

    constexpr insn_t offset_sp_swordoff7(offs_sign_e offsSign, insn_size_t word7) {
        OPSTART;
        bits << word7 << 7
        << offsSign << 1
        << 0b10110000;
        OPEND;
    }

    constexpr insn_t pushpop_reglist(loadstr_e push_or_pop, pushflow_e flow, reglist_t rlist) {
        OPSTART;
        bits << rlist << encoding::REGLIST_BITS
        << flow << 1
        << 0b10 << 2
        << push_or_pop << 1
        << 0b1011;
        OPEND;
    }

    constexpr insn_t multi_ldstr_rlist(loadstr_e op, loreg_t base, reglist_t rlist) {
        OPSTART;
        bits << rlist << encoding::REGLIST_BITS 
        << base << encoding::LOREG_BITS 
        << op << 1
        << 0b1100;
        OPEND;
    }

    constexpr insn_t cond_branch(condition_e cond, insn_size_t soffset8) {
        OPSTART;
        bits << soffset8 << 8
        <<cond << encoding::CONDITION_BITS
        << 0b1101;

        OPEND;
    }

    constexpr insn_t software_interrupt(insn_size_t ival) {
        OPSTART;
        bits << ival << encoding::SWI_BITS << 0b11011111;
        OPEND;
    }

    constexpr insn_t uncond_branch(largebranch_t offset11) {
        OPSTART;
        bits << offset11 << encoding::LARGEBRANCH_BITS 
        << 0b11100;
        OPEND;
    }

    constexpr insn_t branch_with_link(lohigh_offs_e offset_type, largebranch_t offset11 ) {
        OPSTART;
        bits << offset11 << encoding::LARGEBRANCH_BITS 
        << offset_type << 1
        << 0b1111;
        OPEND;
    }

}

#undef OPSTART
#undef OPEND