//====================================================================================
//                       UkWord.h
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

#ifndef _UkWord_h
#define _UkWord_h

#ifdef DEBUG
#define UkWord_DEBUG
#endif
//#define UkWord_DEBUG


//------------------------------------------------------------------------------------
//                       include
//------------------------------------------------------------------------------------

#include <mystd.h>
#include "Markov.h"
#include "StopChar.h"


//------------------------------------------------------------------------------------
//                       class UkWord
//------------------------------------------------------------------------------------

class UkWord
{

  public:

                    UkWord(U_INT4);
    
           void     init();

           void     tran(W_CHAR);

#ifdef _StopChar_h
           U_INT4   lmax() const{return(vallen);};// StopChar ���θ����ͭ����Ĺ��
#endif
    
    inline U_INT4   cost(U_INT4) const;

  private:

           U_INT4   curpos;                       // ���ߤΰ���(ʸƬ�����ʸ����)

           U_INT4   maxlen;                       // Ĺ���κ�����

           U_INT4   wc2int[0x10000];              // ʸ��������֤ؤμ���

           U_INT4   curr;                         // ���ξ���

           Markov   markov;                       // ʸ���ޥ륳�ե�ǥ�

           U_INT4_P Cost;                         // �ư��֤Ǥ��п���Ψ

           U_INT4   UTcost;                       // -log(P(��|UT))*MULT

#ifdef _StopChar_h
           W_CHAR   prev;                         // ľ����ʸ��

           U_INT4   vallen;                       // StopChar ���θ����ͭ����Ĺ��

           StopChar stopchar;                     // ����ʸ��ʬ�䤹��ʸ��
#endif

};

    
//------------------------------------------------------------------------------------
//                       constructor
//------------------------------------------------------------------------------------

UkWord::UkWord(U_INT4 maxlen)
: curpos(0), maxlen(maxlen)
#ifdef _StopChar_h
, prev(U_INT2(0)), vallen(0), stopchar("StopChar")
#endif
{
#ifdef UkWord_DEBUG
    cerr << "UkWord::UkWord(U_INT4)" << endl;
#endif

    curr = BT;

    markov.read("Char.data");

    Cost = new U_INT4[maxlen];

    for (U_INT4 full = 0; full < 0x10000; full++) wc2int[full] = 0;

    U_INT4 lineno;
    ifstream file("CharIntStr.text");
    if (! file) openfailed("CharIntStr.text");
    S_CHAR buff[4];                           // �ե������ɤ߹��ߤΤ���ΥХåե�
    for (lineno = 0; file.read(buff, 3); lineno++){
        assert(buff[2] == '\n');              // ��Ĥ�����ʸ���Ȳ��ԥ����ɤΤϤ�
        W_CHAR wc(buff);
        wc2int[wc.full] = lineno;
    }
    file.close();

    UTcost = U_INT4(log(6878-(lineno-2))*MULT); // ����ʸ���ΰ���ʬ��
}
        
    
//------------------------------------------------------------------------------------
//                       init
//------------------------------------------------------------------------------------

inline void UkWord::init()
{
#ifdef UkWord_DEBUG
    cerr << "UkWord::init()" << endl;
#endif

    curr = BT;

    curpos = 0;
#ifdef _StopChar_h
    vallen = 0;
#endif
}


//------------------------------------------------------------------------------------
//                       tran
//------------------------------------------------------------------------------------

inline void UkWord::tran(const W_CHAR code)
{
#ifdef UkWord_DEBUG
    cerr << "UkWord::tran(const W_CHAR)" << endl;
#endif

#ifdef _StopChar_h
    if (stopchar[code] || stopchar[prev]){        // StopChar ������ľ��ξ��
        vallen = 1;                               // ͭ����Ĺ���򣱤�����
    }else{                                        // ����¾�ξ��
        vallen = min(vallen+1, UkWordMaxLen);     // UkWordMaxLen �ޤǤ��ϰϤ� inc
    }
#endif

    U_INT4 next = wc2int[code.full];              // ���ξ���
#ifdef _StopChar_h
    for (U_INT4 i = max(vallen, curpos)-vallen; i < curpos; i++){
#else
    for (U_INT4 i = max(UkWordMaxLen, curpos)-UkWordMaxLen; i < curpos; i++){
#endif
        Cost[i] += markov.cost(curr, next);
        if (next == UT){                          // ̤��ʸ���ξ��
            Cost[i] += UTcost;
        }
    }
    Cost[curpos] = markov.cost(BT, next);
    if (next == UT){                              // ̤��ʸ���ξ��
        Cost[curpos] += UTcost;
    }
    curr = next;

#ifdef _StopChar_h
    prev = code;
#endif

    curpos++;
}


//------------------------------------------------------------------------------------
//                       cost
//------------------------------------------------------------------------------------

inline U_INT4 UkWord::cost(U_INT4 length) const
{
#ifdef UkWord_DEBUG
    cerr << "UkWord::cost(U_INT4, U_INT4)" << endl;
#endif

    return(Cost[curpos-length]+markov.cost(curr, BT));
}


//------------------------------------------------------------------------------------
//                       endif
//------------------------------------------------------------------------------------

#endif


//====================================================================================
//                       END
//====================================================================================
