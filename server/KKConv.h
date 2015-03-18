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

    IntStr intstr;                                // 品詞番号から品詞表記への応対

    Markov markov;                                // タスクのマルコフモデル

    InDict indict;

    IntStr intext;

    UkWord ukword;                                // 未知語モデル P(x)

#ifdef EXDICT
    ExDict exdict;                                // 外部辞書

    IntStr extext;                                // 外部辞書
#endif // EXDICT

    ExDict tkdict;                                // 単漢字辞書

    IntStr tktext;                                // 単漢字辞書

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

    for (U_INT4 curpos = 0; senten[curpos].half.hi; curpos++){
        tran(senten[curpos]);                     // 辞書などの状態遷移
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

// 機  能 : 入力文字列の変換候補の列挙
//
// 使用法 : list("かんじ")
//
// 注意点 : なし

string KKConv::list(W_String& senten)
{
    init();
    tkdict.init();

    if (senten.length() > SERVERMAXLEN) exit(-1); 
#ifdef KKConv_DEBUG
    cerr << senten << endl;                       // 入力の表示
#endif // KKConv_DEBUG

    for (U_INT4 curpos = 0; senten[curpos].half.hi; curpos++){
        tran(senten[curpos]);                     // 辞書などの状態遷移
        tkdict.tran(senten[curpos]);
    }

    typedef pair<U_INT4, string[2]> PAIR;         // (cost, (word, ORIG))
    typedef multimap<U_INT4, string[2]> MMap;     // cost => PAIR+

    MMap mmap;                                    // (確率, 単語) の順序付きリスト
//    bool flag = FAUX;                             // 平仮名のみからなる単語があるか

    for (InMorp_P morp = indict.lenpos(); morp->length > 0; morp++){
        if (morp->length != senten.length()) continue; // 長さが合わない候補は無視
        U_INT4 cost = markov._1cost(morp->stat) + morp->cost;
        string temp[2] = {intext[morp->text], "IN"};
        mmap.insert(PAIR(cost, temp));

//        W_String tempword(intstr[morp->stat]);
//        if (tempword == senten) flag = VRAI;
    }

#ifdef EXDICT
    for (ExMorp_P morp = exdict.lenpos(); morp->length > 0; morp++){
        if (morp->length != senten.length()) continue; // 長さが合わない候補は無視
        U_INT4 cost = markov._1cost(UT) + morp->cost;
        string temp[2] = {extext[morp->text], "EX"};
        mmap.insert(PAIR(cost, temp));

//        W_String tempword(tktext[morp->text]);
//        if (tempword == senten) flag = VRAI;      // 平仮名のみからなる単語
    }
#endif // EXDICT

    for (ExMorp_P morp = tkdict.lenpos(); morp->length > 0; morp++){
        if (morp->length != senten.length()) continue; // 長さが合わない候補は無視
        U_INT4 cost = markov._1cost(UT) + morp->cost;
        string temp[2] = {tktext[morp->text], "TK"};
        mmap.insert(PAIR(cost, temp));

//        W_String tempword(tktext[morp->text]);
//        if (tempword == senten) flag = VRAI;      // 平仮名のみからなる単語
    }

/*
    if (flag == FAUX){                            // 平仮名のみからなる単語がない場合
        WORD word = STR2WORD(senten, 0, senten.length());
        U_INT4 cost = -log(ukword.prob(word))+markov._1cost(UT);
        string temp[2] = {string(S_CHAR_P(senten)), "UM"};
        mmap.insert(PAIR(cost, temp));
    }
*/
    ostringstream result;                         // 出力する文字列
    result << "(";
    for (MMap::iterator iter = mmap.begin(); iter != mmap.end(); iter++){
        result << "(" << (*iter).first/DECIM8(MULT) << EOT;
        result << (*iter).second[0] << EOT << (*iter).second[1] << ")";
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
}


//------------------------------------------------------------------------------------
//                       dict
//------------------------------------------------------------------------------------

// 機  能 : 辞書検索の結果を表示する
//
// 注意点 : なし

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
