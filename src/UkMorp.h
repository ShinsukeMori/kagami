//====================================================================================
//                       UkMorp.h
//                            by Shinsuke MORI
//                            Last change : 18 May 1996
//====================================================================================

// ��  ǽ : ʸ�� 2-gram �ˤ��̤�θ��ǥ�
//
// ����� : ̤��ʸ����ǥ�Υե����륹�ƥब Part[BT+1 .. PART-1] �˳�Ǽ����Ƥ��뤳��
//          �������������Ĺ���ˤ�����餺 maxlen ʸ���Υơ��֥�����


//------------------------------------------------------------------------------------
//                       define
//------------------------------------------------------------------------------------

#ifndef _UkMorp_h
#define _UkMorp_h

#ifdef DEBUG
#define UkMorp_DEBUG
#endif
//#define UkMorp_DEBUG


//------------------------------------------------------------------------------------
//                       include
//------------------------------------------------------------------------------------

#include <mystd.h>
#include <W_CHAR.h>
#include <Markov.h>


//------------------------------------------------------------------------------------
//                       class UkMorp
//------------------------------------------------------------------------------------

class UkMorp
{

  public:

                    UkMorp(U_INT4);
    
           void     init();
    
           void     tran(W_CHAR);

    inline DECIM8   logP(U_INT4, U_INT4) const;

#ifdef COST
    inline U_INT4   cost(U_INT4, U_INT4) const;
#endif // COST

  private:

           U_INT4   curpos;                       // ���ߤΰ���(ʸƬ�����ʸ����)

           U_INT4   maxlen;                       // Ĺ���κ�����

           U_INT4   wc2int[PART][0x10000];        // ���ʻ��ʸ��������֤ؤμ���

           U_INT4   curr[PART];                   // ���ʻ�κ��ξ���

           Markov   markov[PART];                 // ���ʻ��ʸ���ޥ륳�ե�ǥ�

           DECIM8_P LogP[PART];                   // ���ʻ�γư��֤Ǥ��п���Ψ

           DECIM8   UTlogP[PART];                 // ���ʻ�� -log(P(��|UT))

};

    
//------------------------------------------------------------------------------------
//                       constructor
//------------------------------------------------------------------------------------

UkMorp::UkMorp(U_INT4 maxlen)
: curpos(0), maxlen(maxlen)
{
#ifdef UkMorp_DEBUG
    cerr << "UkMorp::UkMorp(U_INT4)" << endl;
#endif

    for (U_INT4 part = BT+1; part < PART; part++){// ���ʻ���Ф���������
        curr[part] = BT;
        
        string code = Part[part];
        markov[part].dbbind("Char/" + code + "Markov");
        markov[part].setlambda("Char/" + code + "Lambda");

        LogP[part] = new DECIM8[maxlen];

        for (U_INT4 full = 0; full < 0x10000; full++) wc2int[part][full] = 0;

        U_INT4 lineno;
        ifstream file(("Char/" + code + "IntStr.text").c_str());
        if (! file) openfailed("Char/" + code + "IntStr.text");
        S_CHAR buff[4];                           // �ե������ɤ߹��ߤΤ���ΥХåե�
        for (lineno = 0; file.read(buff, 3); lineno++){
            assert(buff[2] == '\n');              // ��Ĥ�����ʸ���Ȳ��ԥ����ɤΤϤ�
            W_CHAR wc(buff);
            wc2int[part][wc.full] = lineno;
        }
        file.close();
        UTlogP[part] = log(6878-(lineno-2));      // 6878 = ɽ���Ǥ�������ʸ���ο�
    }
}
        
    
//------------------------------------------------------------------------------------
//                       init
//------------------------------------------------------------------------------------

inline void UkMorp::init()
{
#ifdef UkMorp_DEBUG
    cerr << "UkMorp::init()" << endl;
#endif

    for (U_INT4 part = BT+1; part < PART; part++){// ���ʻ���Ф���롼��
        curr[part] = BT;
    }

    curpos = 0;
}


//------------------------------------------------------------------------------------
//                       tran
//------------------------------------------------------------------------------------

inline void UkMorp::tran(const W_CHAR code)
{
#ifdef UkMorp_DEBUG
    cerr << "UkMorp::tran(const W_CHAR)" << endl;
#endif

    for (U_INT4 part = BT+1; part < PART; part++){// ���ʻ���Ф���롼��
        U_INT4 next = wc2int[part][code.full];    // ���ξ���
        for (U_INT4 i = 0; i < curpos; i++){
            LogP[part][i] += markov[part].logP(curr[part], next);
            if (next == UT){                      // ̤��ʸ���ξ��
                LogP[part][i] += UTlogP[part];
            }
        }
        LogP[part][curpos] = markov[part].logP(BT, next);
        if (next == UT){                          // ̤��ʸ���ξ��
            LogP[part][curpos] += UTlogP[part];
        }
        curr[part] = next;
    }

    curpos++;
}


//------------------------------------------------------------------------------------
//                       logP
//------------------------------------------------------------------------------------

inline DECIM8 UkMorp::logP(U_INT4 part, U_INT4 length) const
{
#ifdef UkMorp_DEBUG
    cerr << "UkMorp::logP(U_INT4, U_INT4)" << endl;
#endif

    return(LogP[part][curpos-length]+markov[part].logP(curr[part], BT));
}


//------------------------------------------------------------------------------------
//                       cost
//------------------------------------------------------------------------------------

#ifdef COST

inline U_INT4 UkMorp::cost(U_INT4 part, U_INT4 length) const
{
#ifdef UkMorp_DEBUG
    cerr << "UkMorp::cost(U_INT4, U_INT4)" << endl;
#endif

    return(U_INT4(mult*logP(part, length)));
}

#endif // COST


//------------------------------------------------------------------------------------
//                       endif
//------------------------------------------------------------------------------------

#endif


//====================================================================================
//                       END
//====================================================================================
