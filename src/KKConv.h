//====================================================================================
//                       KKConv.h
//                            by Shinsuke MORI
//                            Last change : 24 April 1996
//====================================================================================

// 機  能 : 外部プロセスとの通信のためのバッファー
//
// 注意点 : cf. src-util/agent.c struct connection


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

    IntStr intstr;                                // 品詞番号から品詞表記への応対

    Markov markov;                                // タスクのマルコフモデル

    IntStr taskintstr;                            // 品詞番号から品詞表記への応対

    Markov taskmarkov;                            // タスクのマルコフモデル

    InDict indict;

    InDict taskindict;

#ifdef EXDICT
    ExDict exdict;

    IntStr extext;
#endif // EXDICT

    UkWord ukword;                                // 未知語モデル P(x)

    ExDict tkdict;                                // 単漢字辞書

    IntStr tktext;                                // 単漢字辞書

    VTable vtable;

    S_INT4 C21;                                   // -mlog(L2[1]/L1[1])

    S_INT4        logadd(S_INT4);                 // TaskVTable.h のと統合

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
    DECIM8 L1[3];                                 // ft(w) > 0 の補間係数
    DECIM8 L2[2];                                 // ft(w) = 0 の補間係数
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

// 機  能 : 入力文字列の変換
//
// 使用法 : conv("かんじでかく")

string KKConv::conv(W_String& senten)
{
    init();

    if (senten.length() > SERVERMAXLEN) exit(-1); 
#ifdef KKConv_DEBUG
    cerr << senten << endl;                       // 入力の表示
#endif // KKConv_DEBUG

    for (U_INT4 curpos = 0; senten[curpos].half.hi; curpos++){
        tran(senten[curpos]);                     // 辞書などの状態遷移
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

// 機  能 : 最初の単語境界を指定しての入力文字列の変換
//
// 使用法 : conv("かんじでかく", 3) // ３文字目の直後に境界がある解のみ探索
//
// 注意点 : なし

string KKConv::conv(W_String& senten, U_INT4 fb)
{
    init();

    if (senten.length() > SERVERMAXLEN) exit(-1); 
    if (senten.length() < fb) exit(-1);           // 境界の指定が範囲内か

#ifdef KKConv_DEBUG
    cerr << senten << endl;                       // 入力の表示
#endif // KKConv_DEBUG
    cerr << senten << endl;                       // 入力の表示

    for (U_INT4 curpos = 0; senten[curpos].half.hi; curpos++){
        tran(senten[curpos]);                     // 辞書などの状態遷移
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

// 機  能 : 入力文字列の変換候補の列挙
//
// 使用法 : list("かんじ")
//
// 注意点 : 単語1-gram確率で順序を決定 (要検討; それでよいか, 実装がそうなているか)

string KKConv::list(W_String& senten)
{
    if (senten.length() > SERVERMAXLEN) exit(-1); 
#ifdef KKConv_DEBUG
    cerr << senten << endl;                       // 入力の表示
#endif // KKConv_DEBUG

    init();
    tkdict.init();

    for (U_INT4 curpos = 0; senten[curpos].half.hi; curpos++){
        tran(senten[curpos]);                     // 辞書などの状態遷移
        tkdict.tran(senten[curpos]);
    }

    typedef pair<S_INT4, string[2]> PAIR;         // (cost, (word, ORIG))
    typedef multimap<S_INT4, string[2]> MMap;     // cost => PAIR+
    MMap mmap;                                    // (確率, 単語) の順序付きリスト

    bool flag = FAUX;                             // 平仮名のみからなる単語があるか
    for (InMorp_P morp = indict.lenpos(); morp->length > 0; morp++){
        if (morp->length != senten.length()) continue; // 長さが合わない候補は無視
        S_INT4 Cb1 = markov._1cost(morp->stat);   // -m log λ1Pb1(w)
        S_INT4 Ct1 = taskmarkov._1cost(morp->stat)+C21; // -m log λ1Pt1(w)
        S_INT4 cost = morp->cost+Cb1-logadd(Ct1-Cb1);
        string temp[2] = {intstr[morp->stat], "IN"};
        mmap.insert(PAIR(cost, temp));

        W_String tempword(intstr[morp->stat]);
        if (tempword == senten) flag = VRAI;      // 平仮名のみからなる単語
    }

    for (InMorp_P morp = taskindict.lenpos(); morp->length > 0; morp++){
        if (morp->stat < intstr.size) continue;   // ベースの既知語は対象外(暫定)
        if (morp->length != senten.length()) continue; // 長さが合わない候補は無視
        WORD word = STR2WORD(taskintstr[morp->stat]);
        S_INT4 Cbw = ukword.cost(word);
        S_INT4 Cb1 = markov._1cost(UT)+Cbw;       // -m log λ1Pb1(UT)Mx(w)
        S_INT4 Ct1 = taskmarkov._1cost(morp->stat)+C21; // -m log λ1Pt1(w)
        S_INT4 cost = morp->cost+Cb1-logadd(Ct1-Cb1);
        string temp[2] = {taskintstr[morp->stat], "IV"};
        mmap.insert(PAIR(cost, temp));

        W_String tempword(taskintstr[morp->stat]);
        if (tempword == senten) flag = VRAI;      // 平仮名のみからなる単語
    }

#ifdef EXDICT
    for (ExMorp_P morp = exdict.lenpos(); morp->length > 0; morp++){
        if (morp->length != senten.length()) continue; // 長さが合わない候補は無視
        WORD word = STR2WORD(extext[morp->text]);
//        S_INT4 Cbw = ukword.cost(word);
        S_INT4 Cbw = 0;                           // 要検討
        S_INT4 Cb1 = markov._1cost(UT)+Cbw;       // -m log λ1Pb1(w)
        S_INT4 Ct1 = taskmarkov._1cost(UT)+Cbw+C21;   // -m log λ1Pt1(w)
        S_INT4 cost = morp->cost+Cb1-logadd(Ct1-Cb1);
        string temp[2] = {extext[morp->text], "EX"};
        mmap.insert(PAIR(cost, temp));

        W_String tempword(extext[morp->text]);
        if (tempword == senten) flag = VRAI;      // 平仮名のみからなる単語
    }
#endif // EXDICT

    for (ExMorp_P morp = tkdict.lenpos(); morp->length > 0; morp++){
        if (morp->length != senten.length()) continue; // 長さが合わない候補は無視
        S_INT4 cost = morp->cost;
        string temp[2] = {tktext[morp->text], "TK"};
        mmap.insert(PAIR(cost, temp));

        W_String tempword(tktext[morp->text]);
        if (tempword == senten) flag = VRAI;      // 平仮名のみからなる単語
    }

    if (flag == FAUX){                            // 平仮名のみからなる単語がない場合
        WORD word = STR2WORD(senten, 0, senten.length());
//        S_INT4 Cbw = ukword.cost(word);
//        S_INT4 Cb1 = markov._1cost(UT)+Cbw;       // -m log λ1Pb1(w)
//        S_INT4 Ct1 = taskmarkov._1cost(UT)+Cbw+C21;   // -m log λ1Pt1(w)
//        S_INT4 cost = Cb1-logadd(Ct1-Cb1);
        S_INT4 cost = S_INT4_MAX-1;               // 未知語
        string temp[2] = {string(S_CHAR_P(senten)), "UM"};
        mmap.insert(PAIR(cost, temp));
    }

    ostringstream result;                         // 出力する文字列
    result << "(";
    for (MMap::iterator iter = mmap.begin(); iter != mmap.end(); iter++){
        result << "(" << (*iter).first/log(2)/MULT << EOT
               << (*iter).second[0] << EOT << (*iter).second[1] << ")";
    }
    result << ")";

    return(result.str());
}


//------------------------------------------------------------------------------------

// 機  能 : 入力文字列の変換候補の列挙
//
// 使用法 : list("かんじ")
//
// 注意点 : なし

string KKConv::list(W_String& senten, const string& word)
{
    cerr << "KKConv::list(" << senten << ", " << word << ")" << endl;
    cerr << "Not Implemented!!" << endl;
    return("NULL");

    if (senten.length() > SERVERMAXLEN) exit(-1); 
#ifdef KKConv_DEBUG
    cerr << senten << endl;                       // 入力の表示
#endif // KKConv_DEBUG

    init();
    for (U_INT4 curpos = 0; senten[curpos].half.hi; curpos++){
        tran(senten[curpos]);                     // 辞書などの状態遷移
    }

    typedef pair<S_INT4, string[2]> PAIR;         // (cost, (word, ORIG))
    typedef multimap<S_INT4, string[2]> MMap;     // cost => PAIR+
    MMap mmap;                                    // (確率, 単語) の順序付きリスト

    bool flag = FAUX;                             // 平仮名のみからなる単語があるか
    for (InMorp_P morp = indict.lenpos(); morp->length > 0; morp++){
        if (morp->length != senten.length()) continue; // 長さが合わない候補は無視
        S_INT4 Cb1 = markov._1cost(morp->stat);   // -m log λ1Pb1(w)
        S_INT4 Ct1 = taskmarkov._1cost(morp->stat); // -m log λ1Pt1(w)
        S_INT4 cost = morp->cost+Cb1-logadd(Ct1-Cb1);
        string temp[2] = {intstr[morp->stat], "IN"};
        mmap.insert(PAIR(cost, temp));

        W_String tempword(intstr[morp->stat]);
        if (tempword == senten) flag = VRAI;      // 平仮名のみからなる単語
    }

    for (InMorp_P morp = taskindict.lenpos(); morp->length > 0; morp++){
        if (morp->stat < intstr.size) continue;   // ベースの既知語は対象外(暫定)
        if (morp->length != senten.length()) continue; // 長さが合わない候補は無視
        WORD word = STR2WORD(taskintstr[morp->stat]);
        S_INT4 Cbw = ukword.cost(word);
        S_INT4 Cb1 = markov._1cost(morp->stat)+Cbw; // -m log λ1Pb1(w)
        S_INT4 Ct1 = taskmarkov._1cost(morp->stat); // -m log λ1Pt1(w)
        S_INT4 cost = morp->cost+Cb1-logadd(Ct1-Cb1);
        string temp[2] = {taskintstr[morp->stat], "IV"};
        mmap.insert(PAIR(cost, temp));

        W_String tempword(taskintstr[morp->stat]);
        if (tempword == senten) flag = VRAI;      // 平仮名のみからなる単語
    }

#ifdef EXDICT
    for (ExMorp_P morp = exdict.lenpos(); morp->length > 0; morp++){
        if (morp->length != senten.length()) continue; // 長さが合わない候補は無視
        WORD word = STR2WORD(extext[morp->text]);
//        S_INT4 Cbw = ukword.cost(word);
        S_INT4 Cbw = 0;                           // 要検討
        S_INT4 Cb1 = markov._1cost(UT)+Cbw;       // -m log λ1Pb1(w)
        S_INT4 Ct1 = taskmarkov._1cost(UT)+Cbw;   // -m log λ1Pt1(w)
        S_INT4 cost = morp->cost+Cb1-logadd(Ct1-Cb1);
        string temp[2] = {extext[morp->text], "EX"};
        mmap.insert(PAIR(cost, temp));

        W_String tempword(extext[morp->text]);
        if (tempword == senten) flag = VRAI;      // 平仮名のみからなる単語
    }
#endif // EXDICT

    if (flag == FAUX){                            // 平仮名のみからなる単語がない場合
        WORD word = STR2WORD(senten, 0, senten.length());
        S_INT4 Cbw = ukword.cost(word);
        S_INT4 Cb1 = markov._1cost(UT)+Cbw;       // -m log λ1Pb1(w)
        S_INT4 Ct1 = taskmarkov._1cost(UT)+Cbw;   // -m log λ1Pt1(w)
        S_INT4 cost = Cb1-logadd(Ct1-Cb1);
        string temp[2] = {string(S_CHAR_P(senten)), "UM"};
        mmap.insert(PAIR(cost, temp));
    }

    ostringstream result;                         // 出力する文字列
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

// 機  能 : m*log(1+exp(-x/m)), where m = MULT
//
// 注意点 : いずれテーブルに変更

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
