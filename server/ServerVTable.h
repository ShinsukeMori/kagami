//====================================================================================
//                       VTable.h
//                            by Shinsuke MORI
//                            Last change : 24 April 1996
//====================================================================================

// 機  能 : Viterbi アルゴリズムの表とその要素
//
// 注意点 : vtable[0][BT] と vtable[curpos][BT] が両端となる。


//------------------------------------------------------------------------------------
//                       define
//------------------------------------------------------------------------------------

#ifndef _VTable_h
#define _VTable_h

#ifdef DEBUG
#define VTable_DEBUG
#endif
//#define VTable_DEBUG


//------------------------------------------------------------------------------------
//                       include
//------------------------------------------------------------------------------------

#include <map>
#include <set>

#include <math.h>
#include <mystd.h>
#include <minmax.h>

#include "constant.h"

#include "UkWord.h"
#include "Markov.h"
#include "IntStr.h"
#include "InMorp.h"
#include "ExMorp.h"
#include "DPNode.h"

#include <ExDict.h>
#include <InDict.h>
//#include "UkKKCI.h"


//------------------------------------------------------------------------------------
//                       class VTable
//------------------------------------------------------------------------------------

// 機  能 : Viterbi アルゴリズムの表
//
// 注意点 : Set の配列 DPColumn_P vtable が本体

class VTable{

  public:

                  VTable(const IntStr&, const Markov&, U_INT4);
    
                  ~VTable();

    void          init();                         // 初期化

//    void          fill();                         // ノードを作らない更新

    void          fill(const InDict&, const UkWord&, const U_INT4);
                                                  // 動的計画法の表の更新

    void          fill(const InDict&, const ExDict&, const UkWord&, const U_INT4);
                                                  // 動的計画法の表の更新

    string        output(W_CHAR_P, IntStr&);      // 結果の出力

    string        output(W_CHAR_P, IntStr&, IntStr&);
                                                  // 結果の出力

    void          fprint(ostream&, U_INT4) const; // インスタンスの表示

  private:

    const IntStr& intstr;                         // 単語と数字の対応表

    const Markov& markov;                         // 言語モデル

    const U_INT4  maxlen;                         // 長さの最大値

    typedef map<U_INT4, DPNode> DPColumn;         // 状態番号 => ノード
    typedef DPColumn* DPColumn_P;

    DPColumn_P    vtable;                         // 動的計画法の表

    U_INT4        curpos;			  // 現在の位置(文頭からの文字数)

    void          fill(const InDict&, const U_INT4);// 動的計画法の表の更新(内部辞書)

    void          fill(const ExDict&, const U_INT4);// 動的計画法の表の更新(外部辞書)

    void          fill(const UkWord&, const U_INT4);// 動的計画法の表の更新(未知語)

    void          fill(const U_INT4&, const U_INT4&, const U_INT4&, const S_INT4&,
                       const DPNode::ORIGIN&);    // 動的計画法の表の更新(下請け)

    void          postfill();                     // 枝刈り

};


//------------------------------------------------------------------------------------
//                       constractor
//------------------------------------------------------------------------------------

VTable::VTable(const IntStr& intstr, const Markov& markov, U_INT4 maxlen)
: intstr(intstr), markov(markov), maxlen(maxlen), curpos(0)
{
#ifdef VTable_DEBUG
    cerr << "VTable::VTable(const IntStr&, const Markov&, U_INT4)" << endl;
#endif

    vtable = new DPColumn[maxlen];
    vtable[0][BT] = DPNode(BT);                   // 左端の設定
}


//------------------------------------------------------------------------------------
//                       destractor
//------------------------------------------------------------------------------------

VTable::~VTable()
{
#ifdef VTable_DEBUG
    cerr << "VTable::~VTable()" << endl;
#endif

    delete[] vtable;
}


//------------------------------------------------------------------------------------
//                       init
//------------------------------------------------------------------------------------

void VTable::init()
{
#ifdef VTable_DEBUG
    cerr << "VTable::init()" << endl;
#endif

    for (U_INT4 i = 1; i < curpos+1; i++){
        vtable[i].erase(vtable[i].begin(), vtable[i].end());
    }

    curpos = 0;
}


//------------------------------------------------------------------------------------
//                       fill
//------------------------------------------------------------------------------------

// 機  能 : Viterbi アルゴリズムの表を埋める
//
// 注意点 : なし
/* delete? 2014/10/14
void VTable::fill()
{
#ifdef VTable_DEBUG
    cerr << "VTable::fill(const InDict&, const InDict&, const UkKKCI&)" << endl;
#endif

    curpos++;
}
*/

void VTable::fill(const InDict& indict, const UkWord& ukword, const U_INT4 fb = 0)
{
#ifdef VTable_DEBUG
    cerr << "VTable::fill(const InDict&, const UkWord&, const U_INT4)" << endl;
#endif

    curpos++;

//    if (curpos < fb) return;                      // 1st boundary まではノードを作らず

    fill(indict, fb);
    fill(ukword, fb);

//    postfill(); // 枝狩り(必要?)
}


//------------------------------------------------------------------------------------

void VTable::fill(const InDict& indict, const ExDict& exdict, const UkWord& ukword, const U_INT4 fb = 0)
{
#ifdef VTable_DEBUG
    cerr << "VTable::fill(const InDict&, const UkWord&, const U_INT4)" << endl;
#endif

    curpos++;

//    if (curpos < fb) return;                      // 1st boundary まではノードを作らず

    fill(indict, fb);
    fill(exdict, fb);
    fill(ukword, fb);

//    postfill(); // 枝狩り(必要?)
}


//------------------------------------------------------------------------------------

// 機  能 : Viterbi アルゴリズムの表 vtable[curpos] を内部辞書にある単語で埋める
//
// 注意点 : fb: 1st-boundary 

inline void VTable::fill(const InDict& indict, U_INT4 fb = 0)
{
#ifdef VTable_DEBUG
    cerr << "VTable::Infill(const InDict&)" << endl;
#endif

    for (InMorp_P morp = indict.lenpos(); morp->length > 0; morp++){
        if ((curpos-morp->length < fb)             // fb を跨ぐノードを
            && (fb < curpos)) continue;            // 生成しない
        fill(morp->stat, morp->length, morp->text, morp->cost, DPNode::IN);
    }
}


//------------------------------------------------------------------------------------

// 機  能 : Viterbi アルゴリズムの表 vtable[curpos] を外部辞書にある形態素で埋める
//
// 注意点 : なし

inline void VTable::fill(const ExDict& exdict, U_INT4 fb = 0)
{
#ifdef VTable_DEBUG
    cerr << "VTable::fill(const ExDict&)" << endl;
#endif

    for (ExMorp_P morp = exdict.lenpos(); morp->length > 0; morp++){
        if ((curpos-morp->length < fb)            // fb を跨ぐノードを
            && (fb < curpos)) continue;           // 生成しない
        fill(UT, morp->length, morp->text, morp->cost, DPNode::EX);
    }
}


//------------------------------------------------------------------------------------

// 機  能 : Viterbi アルゴリズムの表 vtable[curpos] を未知入力記号列で埋める
//
// 注意点 : なし

inline void VTable::fill(const UkWord& ukword, U_INT4 fb = 0)
{
#ifdef VTable_DEBUG
    cerr << "VTable::fill(UkWord&, U_INT4)" << endl;
#endif

#ifdef _StopChar_h
    for (U_INT4 length = ukword.lmax(); length > 0; length--){
#else
    for (U_INT4 length = min(curpos, UkWordMaxLen); length > 0; length--){
#endif
        if ((curpos-length < fb)                  // fb を跨ぐノードを
            && (fb < curpos)) continue;           // 生成しない
        fill(UT, length, 0, ukword.cost(length), DPNode::UW);
    }
}


//------------------------------------------------------------------------------------

// 機  能 : Viterbi アルゴリズムの表 vtable[curpos] を形態素 (stat, length) で埋める
//
// 注意点 : なし

inline void VTable::fill(const U_INT4& stat, const U_INT4& length,
                         const U_INT4& text, const S_INT4& Cost,
                         const DPNode::ORIGIN& orig)
{
#ifdef VTable_DEBUG
    cerr << "VTable::fill(...)" << endl;
#endif
    assert(! isnan(Cost));

    for (DPColumn::iterator iter = vtable[curpos-length].begin();
         iter != vtable[curpos-length].end(); iter++){
        S_INT4 cost = (*iter).second.cost+markov.cost((*iter).first, stat)+Cost;
        if ((vtable[curpos][stat].length == 0) ||
            (vtable[curpos][stat].cost > cost)){
            vtable[curpos][stat]
                = DPNode(stat, length, text, orig, cost, &(*iter).second);
        }
    }
}


//------------------------------------------------------------------------------------
//                       postfill
//------------------------------------------------------------------------------------

// 枝狩り

void VTable::postfill()
{
#ifdef VTABLE_DEBUG
    cerr << "VTable::postfill()" << endl;
#endif

    DPColumn& column = vtable[curpos];

    set<U_INT4, greater<U_INT4> > rank;           // 対数確率値の順序列

    for (VTable::DPColumn::iterator iter = column.begin();
         iter != column.end(); iter++){
        rank.insert((*iter).second.cost);
        if (rank.size() > BEAM){                  // 閾値以上の要素があれば
            rank.erase(rank.begin());             // 最大の要素を削除
        }
    }
#ifdef VTABLE_DEBUG
    for (set<U_INT4, greater<U_INT4> >::iterator iter = rank.begin();
         iter != rank.end(); iter++){
        cerr << *iter << endl;
    }
#endif

    for (VTable::DPColumn::iterator iter = column.begin();
         iter != column.end(); iter++){
        if ((*iter).second.cost > *(rank.begin())){
            column.erase(iter);
        }
    }
}


//------------------------------------------------------------------------------------
//                       output
//------------------------------------------------------------------------------------

string VTable::output(W_CHAR_P sent, IntStr& intext)
{
#ifdef VTable_DEBUG
    cerr << "VTable::output(W_CHAR_P, IntStr&)" << endl;
#endif

    DPNode tail;                                  // 終端のノード
    for (DPColumn::iterator iter = vtable[curpos].begin();
         iter != vtable[curpos].end(); iter++){
        S_INT4 cost = (*iter).second.cost+markov.cost((*iter).first, BT);
        if ((tail.stat == UT) || (tail.cost > cost)){
            tail = DPNode(BT, 1, 0, DPNode::IN, cost, &(*iter).second);
        }
    }
    assert(tail.stat == BT);                      // 解がない
    curpos++;

// 後向き探索とメンバ foll の設定
    DPNode_P node;
    for (node = &tail; node->prev != NULL; node = node->prev){
//        cerr << "Text = " << intext[node->text] << endl;
        node->prev->foll = node;
    }

// 前向き探索と最尤解の
    ostringstream result;                         // 出力する文字列
    result << "(" << tail.cost/DECIM8(MULT) << EOT; // 文全体の確率値を表示
    for (node = node->foll; node->foll != NULL; node = node->foll){
        result << "(";
        switch (node->orig){
        case DPNode::IN:
            result << intext[node->text];
            break;
        case DPNode::EX:
            cerr << "メンバ変数 orig = EX が不正です line: " << __LINE__ << endl;
            exit(-1);
        case DPNode::UW:
            result.write((S_CHAR_P)sent, node->length*2);
            break;
        case DPNode::UD:
            cout << "メンバ変数 orig が UD です line: " << __LINE__ << endl;
            exit(-1);
        default:
            cerr << "メンバ変数 orig が不正です: " << node->orig << endl;
            exit(-1);
        }
        result << EOT;
        result.write((S_CHAR_P)sent, node->length*2);
        result << EOT << node->origstring() << ")"; 
        sent += node->length;
    }
    result << ")"; 

    return(result.str());
}


//------------------------------------------------------------------------------------

 string VTable::output(W_CHAR_P sent, IntStr& intext, IntStr& extext)
{
#ifdef VTable_DEBUG
    cerr << "VTable::output(W_CHAR_P, IntStr&)" << endl;
#endif

    DPNode tail;                                  // 終端のノード
    for (DPColumn::iterator iter = vtable[curpos].begin();
         iter != vtable[curpos].end(); iter++){
        S_INT4 cost = (*iter).second.cost+markov.cost((*iter).first, BT);
        if ((tail.stat == UT) || (tail.cost > cost)){
            tail = DPNode(BT, 1, 0, DPNode::IN, cost, &(*iter).second);
        }
    }
    assert(tail.stat == BT);                      // 解がない
    curpos++;

// 後向き探索とメンバ foll の設定
    DPNode_P node;
    for (node = &tail; node->prev != NULL; node = node->prev){
//        cerr << "Text = " << intext[node->text] << endl;
        node->prev->foll = node;
    }

// 前向き探索と最尤解の
    ostringstream result;                         // 出力する文字列
    result << "(" << tail.cost/DECIM8(MULT) << EOT; // 文全体の確率値を表示
    for (node = node->foll; node->foll != NULL; node = node->foll){
        result << "(";
        switch (node->orig){
        case DPNode::IN:
            result << intext[node->text];
            break;
        case DPNode::EX:
            result << extext[node->text];
            break;
        case DPNode::UW:
            result.write((S_CHAR_P)sent, node->length*2);
            break;
        case DPNode::UD:
            cout << "メンバ変数 orig が UD です line: " << __LINE__ << endl;
            exit(-1);
        default:
            cerr << "メンバ変数 orig が不正です: " << node->orig << endl;
            exit(-1);
        }
        result << EOT;
        result.write((S_CHAR_P)sent, node->length*2);
        result << EOT << node->origstring() << ")"; 
        sent += node->length;
    }
    result << ")"; 

    return(result.str());
}


//------------------------------------------------------------------------------------
//                       fprint
//------------------------------------------------------------------------------------

void VTable::fprint(ostream& fout = cout, U_INT4 maxpos = 100) const
{
    fout << "   ";
    for (U_INT4 suf2 = 0; suf2 < maxpos; suf2 += 2) fout << suf2/10 << " ";
    fout << endl;

    fout << "   ";
    for (U_INT4 suf2 = 0; suf2 < maxpos; suf2 += 2) fout << suf2%10 << " ";
    fout << endl;

    fout << "---";
    for (U_INT4 suf2 = 0; suf2 < maxpos; suf2 += 2) fout << "--";
    fout << endl;
/*
    for (U_INT4 suf1 = 0; suf1 < rcintstr.size; suf1++){
        fout << setw(2) << suf1 << " ";
        for (U_INT4 suf2 = 0; suf2 < maxpos; suf2 += 2){
            fout << vtable[suf2][suf1];
        }
        fout << endl;
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
