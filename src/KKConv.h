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
#include "Markov.h"
#include "IntStr.h"
#include "InMorp.h"
#include <InDict.h>
#include "ExMorp.h"
#include <ExDict.h>
#include "TaskVTable.h"


//------------------------------------------------------------------------------------
//                       class KKConv
//------------------------------------------------------------------------------------

class KKConv{

  public:

#ifdef EXDICT
           KKConv(const string&, const string&, const string&);
#else
           KKConv(const string&, const string&);
#endif // EXDICT

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

    IntStr taskintstr;                            // �ʻ��ֹ椫���ʻ�ɽ���ؤα���

    Markov taskmarkov;                            // �������Υޥ륳�ե�ǥ�

    InDict indict;

    InDict taskindict;

#ifdef EXDICT
    ExDict exdict;

    IntStr extext;
#endif // EXDICT

    UkWord ukword;                                // ̤�θ��ǥ� P(x)

    ExDict tkdict;                                // ñ��������

    IntStr tktext;                                // ñ��������

    VTable vtable;

    S_INT4 C21;                                   // -mlog(L2[1]/L1[1])

    S_INT4        logadd(S_INT4);                 // TaskVTable.h �Τ�����

    void   dict(const U_INT4&);

};


//------------------------------------------------------------------------------------
//                       constructor
//------------------------------------------------------------------------------------

#ifdef EXDICT
inline KKConv::KKConv(const string& path, const string& taskstem, const string& dict)
: intstr(path + "WordIntStr"), taskintstr(path + taskstem + "WordIntStr"), 
  markov(path + "Word.data"),  taskmarkov(path + taskstem + "Word.data"), 
  indict(path + "InDict"),     taskindict(path + taskstem + "InDict"),
  exdict(path + dict), extext(path + dict),
  ukword(MAXLEN, path), tkdict(path+"Tankan"), tktext(path+"Tankan"),
  vtable(path, intstr, markov, taskintstr, taskmarkov, ukword, MAXLEN)
{
    ifstream lambda((path + "TaskWordLambda").c_str());
    if (! lambda) openfailed(path + "TaskWordLambda");
    DECIM8 L1[3];                                 // ft(w) > 0 ����ַ���
    DECIM8 L2[2];                                 // ft(w) = 0 ����ַ���
    lambda >> L1[0] >> L1[1] >> L1[2];
    lambda >> L2[0] >> L2[1];
//    cerr << "L1 = (" << L1[0] << ", " << L1[1] << ", " << L1[2] << "), "
//         << "L2 = (" << L2[0] << ", " << L2[1] <<  ")" << endl;
    lambda.close();

    C21 = int(-MULT * (log(L2[1]/L1[1])));

    cerr << "Initialize Done" << endl;
}

#else

inline KKConv::KKConv(const string& stem, const string& taskstem)
: intstr(path + "WordIntStr"), taskintstr(path + taskstem + "WordIntStr"), 
  markov(path + "Word.data"),  taskmarkov(path + taskstem + "Word.data"), 
  indict(path + "InDict"),     taskindict(path + taskstem + "InDict"),
  ukword(MAXLEN, path), tkdict(path+"Tankan"), tktext(path+"Tankan"),
  vtable(intstr, markov, taskintstr, taskmarkov, ukword, MAXLEN)
{
    ifstream lambda((stem+"/TaskWordLambda").c_str());
    if (! lambda) openfailed(stem+"/TaskWordLambda");
    lambda >> L1[0] >> L1[1] >> L1[2];
    lambda >> L2[0] >> L2[1];
//    cerr << "L1 = (" << L1[0] << ", " << L1[1] << ", " << L1[2] << "), "
//         << "L2 = (" << L2[0] << ", " << L2[1] <<  ")" << endl;
    lambda.close();

    C21 = int(-MULT * (log(L2[1]/L1[1])));

    cerr << "Initialize Done" << endl;
}
#endif // EXDICT


//------------------------------------------------------------------------------------
//                       init
//------------------------------------------------------------------------------------

inline void KKConv::init()
{
    //    ukkkci.init();
    ukword.init();
    indict.init();
    taskindict.init();
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
//    ukkkci.tran(wc);
    ukword.tran(wc);
    indict.tran(wc);
    taskindict.tran(wc);
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
//        dictin(indict, intstr, curpos, senten);
//        taskdict(taskindict, taskintstr, curpos, senten);
#endif // KKConv_DEBUG
//        vtable.fill(indict, taskindict, ukkkci);
#ifdef EXDICT
        vtable.fill(indict, taskindict, exdict, ukword);
#else
        vtable.fill(indict, taskindict, ukword);
#endif // EXDICT
    }

#ifdef EXDICT
    return(vtable.result(senten, intstr, taskintstr, extext));
#else
    return(vtable.result(senten, intstr, taskintstr));
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
    cerr << senten << endl;                       // ���Ϥ�ɽ��

    for (U_INT4 curpos = 0; senten[curpos].half.hi; curpos++){
        tran(senten[curpos]);                     // ����ʤɤξ�������
#ifdef KKConv_DEBUG
//        dictin(indict, intstr, curpos, senten);
//        taskdict(taskindict, taskintstr, curpos, senten);
#endif // KKConv_DEBUG
//        vtable.fill(indict, taskindict, ukkkci);
#ifdef EXDICT
        vtable.fill(indict, taskindict, exdict, ukword, fb);
#else
        vtable.fill(indict, taskindict, ukword, fb);
#endif // EXDICT
    }

#ifdef EXDICT
    return(vtable.result(senten, intstr, taskintstr, extext));
#else
    return(vtable.result(senten, intstr, taskintstr));
#endif // EXDICT
}


//------------------------------------------------------------------------------------
//                       list
//------------------------------------------------------------------------------------

// ��  ǽ : ����ʸ������Ѵ���������
//
// ����ˡ : list("����")
//
// ����� : ñ��1-gram��Ψ�ǽ������� (�׸�Ƥ; ����Ǥ褤��, �����������ʤƤ��뤫)

string KKConv::list(W_String& senten)
{
    if (senten.length() > SERVERMAXLEN) exit(-1); 
#ifdef KKConv_DEBUG
    cerr << senten << endl;                       // ���Ϥ�ɽ��
#endif // KKConv_DEBUG

    init();
    tkdict.init();

    for (U_INT4 curpos = 0; senten[curpos].half.hi; curpos++){
        tran(senten[curpos]);                     // ����ʤɤξ�������
        tkdict.tran(senten[curpos]);
    }

    typedef pair<S_INT4, string[2]> PAIR;         // (cost, (word, ORIG))
    typedef multimap<S_INT4, string[2]> MMap;     // cost => PAIR+
    MMap mmap;                                    // (��Ψ, ñ��) �ν���դ��ꥹ��

    bool flag = FAUX;                             // ʿ��̾�Τߤ���ʤ�ñ�줬���뤫
    for (InMorp_P morp = indict.lenpos(); morp->length > 0; morp++){
        if (morp->length != senten.length()) continue; // Ĺ�������ʤ������̵��
        S_INT4 Cb1 = markov._1cost(morp->stat);   // -m log ��1Pb1(w)
        S_INT4 Ct1 = taskmarkov._1cost(morp->stat)+C21; // -m log ��1Pt1(w)
        S_INT4 cost = morp->cost+Cb1-logadd(Ct1-Cb1);
        string temp[2] = {intstr[morp->stat], "IN"};
        mmap.insert(PAIR(cost, temp));

        W_String tempword(intstr[morp->stat]);
        if (tempword == senten) flag = VRAI;      // ʿ��̾�Τߤ���ʤ�ñ��
    }

    for (InMorp_P morp = taskindict.lenpos(); morp->length > 0; morp++){
        if (morp->stat < intstr.size) continue;   // �١����δ��θ���оݳ�(����)
        if (morp->length != senten.length()) continue; // Ĺ�������ʤ������̵��
        WORD word = STR2WORD(taskintstr[morp->stat]);
        S_INT4 Cbw = ukword.cost(word);
        S_INT4 Cb1 = markov._1cost(UT)+Cbw;       // -m log ��1Pb1(UT)Mx(w)
        S_INT4 Ct1 = taskmarkov._1cost(morp->stat)+C21; // -m log ��1Pt1(w)
        S_INT4 cost = morp->cost+Cb1-logadd(Ct1-Cb1);
        string temp[2] = {taskintstr[morp->stat], "IV"};
        mmap.insert(PAIR(cost, temp));

        W_String tempword(taskintstr[morp->stat]);
        if (tempword == senten) flag = VRAI;      // ʿ��̾�Τߤ���ʤ�ñ��
    }

#ifdef EXDICT
    for (ExMorp_P morp = exdict.lenpos(); morp->length > 0; morp++){
        if (morp->length != senten.length()) continue; // Ĺ�������ʤ������̵��
        WORD word = STR2WORD(extext[morp->text]);
//        S_INT4 Cbw = ukword.cost(word);
        S_INT4 Cbw = 0;                           // �׸�Ƥ
        S_INT4 Cb1 = markov._1cost(UT)+Cbw;       // -m log ��1Pb1(w)
        S_INT4 Ct1 = taskmarkov._1cost(UT)+Cbw+C21;   // -m log ��1Pt1(w)
        S_INT4 cost = morp->cost+Cb1-logadd(Ct1-Cb1);
        string temp[2] = {extext[morp->text], "EX"};
        mmap.insert(PAIR(cost, temp));

        W_String tempword(extext[morp->text]);
        if (tempword == senten) flag = VRAI;      // ʿ��̾�Τߤ���ʤ�ñ��
    }
#endif // EXDICT

    for (ExMorp_P morp = tkdict.lenpos(); morp->length > 0; morp++){
        if (morp->length != senten.length()) continue; // Ĺ�������ʤ������̵��
        S_INT4 cost = morp->cost;
        string temp[2] = {tktext[morp->text], "TK"};
        mmap.insert(PAIR(cost, temp));

        W_String tempword(tktext[morp->text]);
        if (tempword == senten) flag = VRAI;      // ʿ��̾�Τߤ���ʤ�ñ��
    }

    if (flag == FAUX){                            // ʿ��̾�Τߤ���ʤ�ñ�줬�ʤ����
        WORD word = STR2WORD(senten, 0, senten.length());
//        S_INT4 Cbw = ukword.cost(word);
//        S_INT4 Cb1 = markov._1cost(UT)+Cbw;       // -m log ��1Pb1(w)
//        S_INT4 Ct1 = taskmarkov._1cost(UT)+Cbw+C21;   // -m log ��1Pt1(w)
//        S_INT4 cost = Cb1-logadd(Ct1-Cb1);
        S_INT4 cost = S_INT4_MAX-1;               // ̤�θ�
        string temp[2] = {string(S_CHAR_P(senten)), "UM"};
        mmap.insert(PAIR(cost, temp));
    }

    ostringstream result;                         // ���Ϥ���ʸ����
    result << "(";
    for (MMap::iterator iter = mmap.begin(); iter != mmap.end(); iter++){
        result << "(" << (*iter).first/log(2)/MULT << EOT
               << (*iter).second[0] << EOT << (*iter).second[1] << ")";
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

    if (senten.length() > SERVERMAXLEN) exit(-1); 
#ifdef KKConv_DEBUG
    cerr << senten << endl;                       // ���Ϥ�ɽ��
#endif // KKConv_DEBUG

    init();
    for (U_INT4 curpos = 0; senten[curpos].half.hi; curpos++){
        tran(senten[curpos]);                     // ����ʤɤξ�������
    }

    typedef pair<S_INT4, string[2]> PAIR;         // (cost, (word, ORIG))
    typedef multimap<S_INT4, string[2]> MMap;     // cost => PAIR+
    MMap mmap;                                    // (��Ψ, ñ��) �ν���դ��ꥹ��

    bool flag = FAUX;                             // ʿ��̾�Τߤ���ʤ�ñ�줬���뤫
    for (InMorp_P morp = indict.lenpos(); morp->length > 0; morp++){
        if (morp->length != senten.length()) continue; // Ĺ�������ʤ������̵��
        S_INT4 Cb1 = markov._1cost(morp->stat);   // -m log ��1Pb1(w)
        S_INT4 Ct1 = taskmarkov._1cost(morp->stat); // -m log ��1Pt1(w)
        S_INT4 cost = morp->cost+Cb1-logadd(Ct1-Cb1);
        string temp[2] = {intstr[morp->stat], "IN"};
        mmap.insert(PAIR(cost, temp));

        W_String tempword(intstr[morp->stat]);
        if (tempword == senten) flag = VRAI;      // ʿ��̾�Τߤ���ʤ�ñ��
    }

    for (InMorp_P morp = taskindict.lenpos(); morp->length > 0; morp++){
        if (morp->stat < intstr.size) continue;   // �١����δ��θ���оݳ�(����)
        if (morp->length != senten.length()) continue; // Ĺ�������ʤ������̵��
        WORD word = STR2WORD(taskintstr[morp->stat]);
        S_INT4 Cbw = ukword.cost(word);
        S_INT4 Cb1 = markov._1cost(morp->stat)+Cbw; // -m log ��1Pb1(w)
        S_INT4 Ct1 = taskmarkov._1cost(morp->stat); // -m log ��1Pt1(w)
        S_INT4 cost = morp->cost+Cb1-logadd(Ct1-Cb1);
        string temp[2] = {taskintstr[morp->stat], "IV"};
        mmap.insert(PAIR(cost, temp));

        W_String tempword(taskintstr[morp->stat]);
        if (tempword == senten) flag = VRAI;      // ʿ��̾�Τߤ���ʤ�ñ��
    }

#ifdef EXDICT
    for (ExMorp_P morp = exdict.lenpos(); morp->length > 0; morp++){
        if (morp->length != senten.length()) continue; // Ĺ�������ʤ������̵��
        WORD word = STR2WORD(extext[morp->text]);
//        S_INT4 Cbw = ukword.cost(word);
        S_INT4 Cbw = 0;                           // �׸�Ƥ
        S_INT4 Cb1 = markov._1cost(UT)+Cbw;       // -m log ��1Pb1(w)
        S_INT4 Ct1 = taskmarkov._1cost(UT)+Cbw;   // -m log ��1Pt1(w)
        S_INT4 cost = morp->cost+Cb1-logadd(Ct1-Cb1);
        string temp[2] = {extext[morp->text], "EX"};
        mmap.insert(PAIR(cost, temp));

        W_String tempword(extext[morp->text]);
        if (tempword == senten) flag = VRAI;      // ʿ��̾�Τߤ���ʤ�ñ��
    }
#endif // EXDICT

    if (flag == FAUX){                            // ʿ��̾�Τߤ���ʤ�ñ�줬�ʤ����
        WORD word = STR2WORD(senten, 0, senten.length());
        S_INT4 Cbw = ukword.cost(word);
        S_INT4 Cb1 = markov._1cost(UT)+Cbw;       // -m log ��1Pb1(w)
        S_INT4 Ct1 = taskmarkov._1cost(UT)+Cbw;   // -m log ��1Pt1(w)
        S_INT4 cost = Cb1-logadd(Ct1-Cb1);
        string temp[2] = {string(S_CHAR_P(senten)), "UM"};
        mmap.insert(PAIR(cost, temp));
    }

    ostringstream result;                         // ���Ϥ���ʸ����
    result << "(";
    for (MMap::iterator iter = mmap.begin(); iter != mmap.end(); iter++){
        result << "(" << (*iter).first/log(2)/MULT << EOT
               << (*iter).second[0] << EOT << (*iter).second[1] << ")";
    }
    result << ")";

    return(result.str());
}


//------------------------------------------------------------------------------------
//                       logadd
//------------------------------------------------------------------------------------

// ��  ǽ : m*log(1+exp(-x/m)), where m = MULT
//
// ����� : ������ơ��֥���ѹ�

inline S_INT4 KKConv::logadd(S_INT4 x)
{
#ifdef VTable_DEBUG
    cerr << "VTable::logadd(S_INT4)" << endl;
#endif

//    return(0);
    return(int(MULT*log(1+exp(-DECIM8(x)/MULT))));
}


//------------------------------------------------------------------------------------
//                       dict
//------------------------------------------------------------------------------------

void KKConv::dict(const U_INT4& curpos)
{
    /*
    for (InMorp_P morp = indict.lenpos() ;morp->length > 0; morp++){
        for (U_INT2 pos = 0; pos+morp->length <= curpos; pos++) cerr << "  ";
        cerr.write((S_CHAR_P)(senten+curpos+1-morp->length), morp->length*2) << "/";
        cerr << intstr[morp->stat] << "/IN " << morp->logP << endl;
    }
    for (InMorp_P morp = taskindict.lenpos(); morp->length > 0; morp++){
        for (U_INT2 pos = 0; pos+morp->length <= curpos; pos++) cerr << "  ";
        cerr.write((S_CHAR_P)(senten+curpos+1-morp->length), morp->length*2) << "/";
        cerr << taskintstr[morp->stat] << "/TI " << morp->logP << endl;
    }
    */
}


//------------------------------------------------------------------------------------
//                       endif
//------------------------------------------------------------------------------------

#endif


//====================================================================================
//                       END
//====================================================================================
