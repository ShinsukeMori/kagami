//====================================================================================
//                       DPNode.h
//                            by Shinsuke MORI
//                            Last change : 24 April 1996
//====================================================================================

// 機  能 : Viterbi アルゴリズムのノード
//
// 注意点 : VTableWithIV.h でのみ使用


//------------------------------------------------------------------------------------
//                       define
//------------------------------------------------------------------------------------

#ifndef _DPNode_h
#define _DPNode_h 1

#ifdef DEBUG
#define DPNode_DEBUG
#endif
//#define DPNode_DEBUG


//------------------------------------------------------------------------------------
//                       include
//------------------------------------------------------------------------------------

#include <mystd.h>
//#include <Word.h>


//------------------------------------------------------------------------------------
//                       class DPNode
//------------------------------------------------------------------------------------

class DPNode;
typedef DPNode* DPNode_P;
typedef DPNode** DPNode_P_P;

class DPNode{

  public:

    U_INT4   stat;                                // 状態番号

    U_INT4   length;                              // 長さ

    U_INT4   text;                                // 変換後の表記

    enum ORIGIN { UD /*未定*/, IN /*内部辞書*/, EX /*外部辞書*/, UW /*未知語*/,
                  IV /*生コ単語*/, };

    ORIGIN   orig;                                // 出処

    S_INT4   cost;                                // 累積確率

    DPNode_P prev;                                // 直前のノード

    DPNode_P foll;                                // 直後のノード

             DPNode();

             DPNode(U_INT4);

             DPNode(U_INT4, U_INT4, U_INT4, ORIGIN, S_INT4, DPNode_P);

    void     init();

    string   origstring();

#ifdef VTable_DEBUG
friend ostream& operator<<(ostream&, const IntStr&);
#endif

  private:

};


//------------------------------------------------------------------------------------
//                       constructor
//------------------------------------------------------------------------------------

inline DPNode::DPNode()
: stat(0), length(0), text(0), orig(UD), cost(0), prev(NULL), foll(NULL)
{
    ;                                             // No Operation
}

inline DPNode::DPNode(U_INT4 stat)
: stat(stat), length(0), text(0), orig(IN), cost(0), prev(NULL), foll(NULL)
{
    ;                                             // No Operation
}

inline DPNode::DPNode(U_INT4 stat, U_INT4 length, U_INT4 text, ORIGIN orig,
                      S_INT4 cost, DPNode_P prev)
: stat(stat), length(length), text(text), orig(orig), cost(cost), prev(prev),
  foll(NULL)
{
    ;                                             // No Operation
}


//------------------------------------------------------------------------------------
//                       init
//------------------------------------------------------------------------------------

inline void DPNode::init()
{
    stat = 0;
    length = 0;
    orig = UD;
    cost = 0;
    prev = NULL;
    foll = NULL;
}


//------------------------------------------------------------------------------------
//                       origstring
//------------------------------------------------------------------------------------

inline string DPNode::origstring()
{
    switch (orig){
    case DPNode::IN:
        return(string("IN"));
    case DPNode::EX:
        return(string("EX"));
    case DPNode::UW:
        return(string("UW"));
    case DPNode::IV:
        return(string("IV"));
    case DPNode::UD:
        return(string("UD"));
    default:
        cerr << "引数 orig が不正です: " << endl;
    }
}


//------------------------------------------------------------------------------------
//                       operator<<
//------------------------------------------------------------------------------------

inline ostream& operator<<(ostream& s, const DPNode& node)
{
    s << "(" << setw(2) << node.length << "," << setw(2) << node.stat << ")";
    return(s);
}


//------------------------------------------------------------------------------------

inline ostream& operator<<(ostream& s, const DPNode::ORIGIN& orig)
{
#ifdef VTable_DEBUG
    cerr << "operator<<(ostream&, const DPNode::ORIGIN&)" << endl;
#endif

    switch (orig){
    case DPNode::IN:
        cout << "IN";
        break;
    case DPNode::EX:
        cout << "EX";
        break;
    case DPNode::UW:
        cout << "UW";
        break;
    case DPNode::UD:
        cout << "UD";
        break;
    default:
        cerr << "引数 orig が不正です: " << U_INT4(orig) << endl;
        exit(-1);
    }

    return(s);
}


//------------------------------------------------------------------------------------
//                       endif
//------------------------------------------------------------------------------------

#endif


//====================================================================================
//                       END
//====================================================================================
