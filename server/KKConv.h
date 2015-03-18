//====================================================================================
//                       KKConv.h
//                            by Shinsuke MORI
//                            Last change : 24 April 1996
//====================================================================================

// ��  ǽ : �����ץ����Ȥ��̿��Τ���ΥХåե���
//
// ����� : cf. src-util/agent.c struct connection


//------------------------------------------------------------------------------------
//                       define
//------------------------------------------------------------------------------------

#ifndef _KKConv_h
#define _KKConv_h

#ifdef DEBUG
#define KKConv_DEBUG
#endif
//#define KKConv_DEBUG


//------------------------------------------------------------------------------------
//                       include
//------------------------------------------------------------------------------------

#include <math.h>
#include <mystd.h>
#include <minmax.h>

#include "UkWord.h"
//#include "UkKKCI.h"
#include "Markov.h"
#include "IntStr.h"
#include "InMorp.h"
#include "ExMorp.h"
#include "ExDict.h"
#include "ServerVTable.h"


//------------------------------------------------------------------------------------
//                       class KKConv
//------------------------------------------------------------------------------------

class KKConv{

  public:

           KKConv(const string&);

    void   init();

    void   tran(const W_CHAR&);

    string conv(W_String&);

    string conv(W_String&, U_INT4);

    string list(W_String&);

    string list(W_String&, const string&);

friend ostream& operator<<(ostream&, const KKConv&);

friend istream& operator>>(istream&, KKConv&);

  private:

    IntStr intstr;                                // �ʻ��ֹ椫���ʻ�ɽ���ؤα���

    Markov markov;                                // �������Υޥ륳�ե�ǥ�

    InDict indict;

    IntStr intext;

    UkWord ukword;                                // ̤�θ��ǥ� P(x)

#ifdef EXDICT
    ExDict exdict;                                // ��������

    IntStr extext;                                // ��������
#endif // EXDICT

    ExDict tkdict;                                // ñ��������

    IntStr tktext;                                // ñ��������

    VTable vtable;

    void   dict(const InDict&, const IntStr&, const IntStr&, const U_INT4, const W_CHAR_P);

    void   dict(const ExDict&, const IntStr&, const U_INT4, const W_CHAR_P);

};


//------------------------------------------------------------------------------------
//                       constructor
//------------------------------------------------------------------------------------

inline KKConv::KKConv(const string& stem)
: intstr(stem + "ClstIntStr"), markov(stem + "Clst.data"), 
  indict(stem + "InDict"), intext(stem + "WordIntStr"), 
  ukword(MAXLEN),
#ifdef EXDICT
  exdict(stem + "ExDict"), extext(stem + "ExDict"), 
#endif // EXDICT
  tkdict(stem + "Tankan"), tktext(stem + "Tankan"), 
  vtable(intstr, markov, MAXLEN)
{
    ;                                             // NOP
    cerr << "Initialize Done" << endl;
}


//------------------------------------------------------------------------------------
//                       init
//------------------------------------------------------------------------------------

inline void KKConv::init()
{
    ukword.init();
    indict.init();
#ifdef EXDICT
    exdict.init();
#endif // EXDICT

    vtable.init();
}


//------------------------------------------------------------------------------------
//                       tran
//------------------------------------------------------------------------------------

inline void KKConv::tran(const W_CHAR& wc)
{
    ukword.tran(wc);
    indict.tran(wc);
#ifdef EXDICT
    exdict.tran(wc);
#endif // EXDICT
}


//------------------------------------------------------------------------------------
//                       conv
//------------------------------------------------------------------------------------

// ��  ǽ : ����ʸ������Ѵ�
//
// ����ˡ : conv("���󤸤Ǥ���")

string KKConv::conv(W_String& senten)
{
    init();

    if (senten.length() > SERVERMAXLEN) exit(-1); 
#ifdef KKConv_DEBUG
    cerr << senten << endl;                       // ���Ϥ�ɽ��
#endif // KKConv_DEBUG

    for (U_INT4 curpos = 0; senten[curpos].half.hi; curpos++){
        tran(senten[curpos]);                     // ����ʤɤξ�������
#ifdef KKConv_DEBUG
        dict(indict, intext, intstr, curpos, senten);
#ifdef EXDICT
        dict(exdict, extext, curpos, senten);
#endif // EXDICT
#endif // KKConv_DEBUG

#ifdef EXDICT
        vtable.fill(indict, exdict, ukword);
#else
        vtable.fill(indict, ukword);
#endif // EXDICT
    }

#ifdef EXDICT
    return(vtable.output(senten, intext, extext));
#else
    return(vtable.output(senten, intext));
#endif // EXDICT
}


//------------------------------------------------------------------------------------

// ��  ǽ : �ǽ��ñ�춭������ꤷ�Ƥ�����ʸ������Ѵ�
//
// ����ˡ : conv("���󤸤Ǥ���", 3) // ��ʸ���ܤ�ľ��˶����������Τ�õ��
//
// ����� : �ʤ�

string KKConv::conv(W_String& senten, U_INT4 fb)
{
    init();

    if (senten.length() > SERVERMAXLEN) exit(-1); 
    if (senten.length() < fb) exit(-1);           // �����λ��꤬�ϰ��⤫

#ifdef KKConv_DEBUG
    cerr << senten << endl;                       // ���Ϥ�ɽ��
#endif // KKConv_DEBUG

    for (U_INT4 curpos = 0; senten[curpos].half.hi; curpos++){
        tran(senten[curpos]);                     // ����ʤɤξ�������
#ifdef KKConv_DEBUG
        dict(indict, intext, intstr, curpos, senten);
#endif // KKConv_DEBUG
#ifdef EXDICT
        vtable.fill(indict, exdict, ukword, fb);
#else
        vtable.fill(indict, ukword, fb);
#endif // EXDICT
    }

#ifdef EXDICT
    return(vtable.output(senten, intext, extext));
#else
    return(vtable.output(senten, intext));
#endif // EXDICT
}


//------------------------------------------------------------------------------------
//                       list
//------------------------------------------------------------------------------------

// ��  ǽ : ����ʸ������Ѵ���������
//
// ����ˡ : list("����")
//
// ����� : �ʤ�

string KKConv::list(W_String& senten)
{
    init();
    tkdict.init();

    if (senten.length() > SERVERMAXLEN) exit(-1); 
#ifdef KKConv_DEBUG
    cerr << senten << endl;                       // ���Ϥ�ɽ��
#endif // KKConv_DEBUG

    for (U_INT4 curpos = 0; senten[curpos].half.hi; curpos++){
        tran(senten[curpos]);                     // ����ʤɤξ�������
        tkdict.tran(senten[curpos]);
    }

    typedef pair<U_INT4, string[2]> PAIR;         // (cost, (word, ORIG))
    typedef multimap<U_INT4, string[2]> MMap;     // cost => PAIR+

    MMap mmap;                                    // (��Ψ, ñ��) �ν���դ��ꥹ��
//    bool flag = FAUX;                             // ʿ��̾�Τߤ���ʤ�ñ�줬���뤫

    for (InMorp_P morp = indict.lenpos(); morp->length > 0; morp++){
        if (morp->length != senten.length()) continue; // Ĺ�������ʤ������̵��
        U_INT4 cost = markov._1cost(morp->stat) + morp->cost;
        string temp[2] = {intext[morp->text], "IN"};
        mmap.insert(PAIR(cost, temp));

//        W_String tempword(intstr[morp->stat]);
//        if (tempword == senten) flag = VRAI;
    }

#ifdef EXDICT
    for (ExMorp_P morp = exdict.lenpos(); morp->length > 0; morp++){
        if (morp->length != senten.length()) continue; // Ĺ�������ʤ������̵��
        U_INT4 cost = markov._1cost(UT) + morp->cost;
        string temp[2] = {extext[morp->text], "EX"};
        mmap.insert(PAIR(cost, temp));

//        W_String tempword(tktext[morp->text]);
//        if (tempword == senten) flag = VRAI;      // ʿ��̾�Τߤ���ʤ�ñ��
    }
#endif // EXDICT

    for (ExMorp_P morp = tkdict.lenpos(); morp->length > 0; morp++){
        if (morp->length != senten.length()) continue; // Ĺ�������ʤ������̵��
        U_INT4 cost = markov._1cost(UT) + morp->cost;
        string temp[2] = {tktext[morp->text], "TK"};
        mmap.insert(PAIR(cost, temp));

//        W_String tempword(tktext[morp->text]);
//        if (tempword == senten) flag = VRAI;      // ʿ��̾�Τߤ���ʤ�ñ��
    }

/*
    if (flag == FAUX){                            // ʿ��̾�Τߤ���ʤ�ñ�줬�ʤ����
        WORD word = STR2WORD(senten, 0, senten.length());
        U_INT4 cost = -log(ukword.prob(word))+markov._1cost(UT);
        string temp[2] = {string(S_CHAR_P(senten)), "UM"};
        mmap.insert(PAIR(cost, temp));
    }
*/
    ostringstream result;                         // ���Ϥ���ʸ����
    result << "(";
    for (MMap::iterator iter = mmap.begin(); iter != mmap.end(); iter++){
        result << "(" << (*iter).first/DECIM8(MULT) << EOT;
        result << (*iter).second[0] << EOT << (*iter).second[1] << ")";
    }
    result << ")";

    return(result.str());
}


//------------------------------------------------------------------------------------

// ��  ǽ : ����ʸ������Ѵ���������
//
// ����ˡ : list("����")
//
// ����� : �ʤ�

string KKConv::list(W_String& senten, const string& word)
{
    cerr << "KKConv::list(" << senten << ", " << word << ")" << endl;
    cerr << "Not Implemented!!" << endl;
    return("NULL");
}


//------------------------------------------------------------------------------------
//                       dict
//------------------------------------------------------------------------------------

// ��  ǽ : ���񸡺��η�̤�ɽ������
//
// ����� : �ʤ�

void KKConv::dict(const InDict& indict, const IntStr& intext, const IntStr& intstr,
                  const U_INT4 curpos, const W_CHAR_P senten)

{
    for (InMorp_P morp = indict.lenpos() ;morp->length > 0; morp++){
        for (U_INT2 pos = 0; pos+morp->length <= curpos; pos++) cerr << "  ";
        cerr.write((S_CHAR_P)(senten+curpos+1-morp->length), morp->length*2) << "/";
        cerr << intstr[morp->stat] << "/" << intext[morp->text] << endl;
    }
    for (ExMorp_P morp = tkdict.lenpos(); morp->length > 0; morp++){
        for (U_INT2 pos = 0; pos+morp->length <= curpos; pos++) cerr << "  ";
        cerr << tktext[morp->text] << "/TK " << morp->cost << endl;
    }
}


//------------------------------------------------------------------------------------

void KKConv::dict(const ExDict& exdict, const IntStr& extext, 
                  const U_INT4 curpos, const W_CHAR_P senten)
{
    for (ExMorp_P morp = exdict.lenpos(); morp->length > 0; morp++){
        for (U_INT2 pos = 0; pos+morp->length <= curpos; pos++) cerr << "  ";
        cerr.write((S_CHAR_P)(senten+curpos+1-morp->length), morp->length*2) << "/";
        cerr << "EX/" << extext[morp->text] << endl;
    }
}


//------------------------------------------------------------------------------------
//                       endif
//------------------------------------------------------------------------------------

#endif


//====================================================================================
//                       END
//====================================================================================
