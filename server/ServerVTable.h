//====================================================================================
//                       VTable.h
//                            by Shinsuke MORI
//                            Last change : 24 April 1996
//====================================================================================

// ��  ǽ : Viterbi ���르�ꥺ���ɽ�Ȥ�������
//
// ����� : vtable[0][BT] �� vtable[curpos][BT] ��ξü�Ȥʤ롣


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

// ��  ǽ : Viterbi ���르�ꥺ���ɽ
//
// ����� : Set ������ DPColumn_P vtable ������

class VTable{

  public:

                  VTable(const IntStr&, const Markov&, U_INT4);
    
                  ~VTable();

    void          init();                         // �����

//    void          fill();                         // �Ρ��ɤ���ʤ�����

    void          fill(const InDict&, const UkWord&, const U_INT4);
                                                  // ưŪ�ײ�ˡ��ɽ�ι���

    void          fill(const InDict&, const ExDict&, const UkWord&, const U_INT4);
                                                  // ưŪ�ײ�ˡ��ɽ�ι���

    string        output(W_CHAR_P, IntStr&);      // ��̤ν���

    string        output(W_CHAR_P, IntStr&, IntStr&);
                                                  // ��̤ν���

    void          fprint(ostream&, U_INT4) const; // ���󥹥��󥹤�ɽ��

  private:

    const IntStr& intstr;                         // ñ��ȿ������б�ɽ

    const Markov& markov;                         // �����ǥ�

    const U_INT4  maxlen;                         // Ĺ���κ�����

    typedef map<U_INT4, DPNode> DPColumn;         // �����ֹ� => �Ρ���
    typedef DPColumn* DPColumn_P;

    DPColumn_P    vtable;                         // ưŪ�ײ�ˡ��ɽ

    U_INT4        curpos;			  // ���ߤΰ���(ʸƬ�����ʸ����)

    void          fill(const InDict&, const U_INT4);// ưŪ�ײ�ˡ��ɽ�ι���(��������)

    void          fill(const ExDict&, const U_INT4);// ưŪ�ײ�ˡ��ɽ�ι���(��������)

    void          fill(const UkWord&, const U_INT4);// ưŪ�ײ�ˡ��ɽ�ι���(̤�θ�)

    void          fill(const U_INT4&, const U_INT4&, const U_INT4&, const S_INT4&,
                       const DPNode::ORIGIN&);    // ưŪ�ײ�ˡ��ɽ�ι���(������)

    void          postfill();                     // �޴���

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
    vtable[0][BT] = DPNode(BT);                   // ��ü������
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

// ��  ǽ : Viterbi ���르�ꥺ���ɽ������
//
// ����� : �ʤ�
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

//    if (curpos < fb) return;                      // 1st boundary �ޤǤϥΡ��ɤ��餺

    fill(indict, fb);
    fill(ukword, fb);

//    postfill(); // �޼��(ɬ��?)
}


//------------------------------------------------------------------------------------

void VTable::fill(const InDict& indict, const ExDict& exdict, const UkWord& ukword, const U_INT4 fb = 0)
{
#ifdef VTable_DEBUG
    cerr << "VTable::fill(const InDict&, const UkWord&, const U_INT4)" << endl;
#endif

    curpos++;

//    if (curpos < fb) return;                      // 1st boundary �ޤǤϥΡ��ɤ��餺

    fill(indict, fb);
    fill(exdict, fb);
    fill(ukword, fb);

//    postfill(); // �޼��(ɬ��?)
}


//------------------------------------------------------------------------------------

// ��  ǽ : Viterbi ���르�ꥺ���ɽ vtable[curpos] ����������ˤ���ñ�������
//
// ����� : fb: 1st-boundary 

inline void VTable::fill(const InDict& indict, U_INT4 fb = 0)
{
#ifdef VTable_DEBUG
    cerr << "VTable::Infill(const InDict&)" << endl;
#endif

    for (InMorp_P morp = indict.lenpos(); morp->length > 0; morp++){
        if ((curpos-morp->length < fb)             // fb ��٤��Ρ��ɤ�
            && (fb < curpos)) continue;            // �������ʤ�
        fill(morp->stat, morp->length, morp->text, morp->cost, DPNode::IN);
    }
}


//------------------------------------------------------------------------------------

// ��  ǽ : Viterbi ���르�ꥺ���ɽ vtable[curpos] ��������ˤ�������Ǥ�����
//
// ����� : �ʤ�

inline void VTable::fill(const ExDict& exdict, U_INT4 fb = 0)
{
#ifdef VTable_DEBUG
    cerr << "VTable::fill(const ExDict&)" << endl;
#endif

    for (ExMorp_P morp = exdict.lenpos(); morp->length > 0; morp++){
        if ((curpos-morp->length < fb)            // fb ��٤��Ρ��ɤ�
            && (fb < curpos)) continue;           // �������ʤ�
        fill(UT, morp->length, morp->text, morp->cost, DPNode::EX);
    }
}


//------------------------------------------------------------------------------------

// ��  ǽ : Viterbi ���르�ꥺ���ɽ vtable[curpos] ��̤�����ϵ����������
//
// ����� : �ʤ�

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
        if ((curpos-length < fb)                  // fb ��٤��Ρ��ɤ�
            && (fb < curpos)) continue;           // �������ʤ�
        fill(UT, length, 0, ukword.cost(length), DPNode::UW);
    }
}


//------------------------------------------------------------------------------------

// ��  ǽ : Viterbi ���르�ꥺ���ɽ vtable[curpos] ������� (stat, length) ������
//
// ����� : �ʤ�

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

// �޼��

void VTable::postfill()
{
#ifdef VTABLE_DEBUG
    cerr << "VTable::postfill()" << endl;
#endif

    DPColumn& column = vtable[curpos];

    set<U_INT4, greater<U_INT4> > rank;           // �п���Ψ�ͤν����

    for (VTable::DPColumn::iterator iter = column.begin();
         iter != column.end(); iter++){
        rank.insert((*iter).second.cost);
        if (rank.size() > BEAM){                  // ���Ͱʾ�����Ǥ������
            rank.erase(rank.begin());             // ��������Ǥ���
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

    DPNode tail;                                  // ��ü�ΥΡ���
    for (DPColumn::iterator iter = vtable[curpos].begin();
         iter != vtable[curpos].end(); iter++){
        S_INT4 cost = (*iter).second.cost+markov.cost((*iter).first, BT);
        if ((tail.stat == UT) || (tail.cost > cost)){
            tail = DPNode(BT, 1, 0, DPNode::IN, cost, &(*iter).second);
        }
    }
    assert(tail.stat == BT);                      // �򤬤ʤ�
    curpos++;

// �����õ���ȥ��� foll ������
    DPNode_P node;
    for (node = &tail; node->prev != NULL; node = node->prev){
//        cerr << "Text = " << intext[node->text] << endl;
        node->prev->foll = node;
    }

// ������õ���Ⱥ�����
    ostringstream result;                         // ���Ϥ���ʸ����
    result << "(" << tail.cost/DECIM8(MULT) << EOT; // ʸ���Τγ�Ψ�ͤ�ɽ��
    for (node = node->foll; node->foll != NULL; node = node->foll){
        result << "(";
        switch (node->orig){
        case DPNode::IN:
            result << intext[node->text];
            break;
        case DPNode::EX:
            cerr << "�����ѿ� orig = EX �������Ǥ� line: " << __LINE__ << endl;
            exit(-1);
        case DPNode::UW:
            result.write((S_CHAR_P)sent, node->length*2);
            break;
        case DPNode::UD:
            cout << "�����ѿ� orig �� UD �Ǥ� line: " << __LINE__ << endl;
            exit(-1);
        default:
            cerr << "�����ѿ� orig �������Ǥ�: " << node->orig << endl;
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

    DPNode tail;                                  // ��ü�ΥΡ���
    for (DPColumn::iterator iter = vtable[curpos].begin();
         iter != vtable[curpos].end(); iter++){
        S_INT4 cost = (*iter).second.cost+markov.cost((*iter).first, BT);
        if ((tail.stat == UT) || (tail.cost > cost)){
            tail = DPNode(BT, 1, 0, DPNode::IN, cost, &(*iter).second);
        }
    }
    assert(tail.stat == BT);                      // �򤬤ʤ�
    curpos++;

// �����õ���ȥ��� foll ������
    DPNode_P node;
    for (node = &tail; node->prev != NULL; node = node->prev){
//        cerr << "Text = " << intext[node->text] << endl;
        node->prev->foll = node;
    }

// ������õ���Ⱥ�����
    ostringstream result;                         // ���Ϥ���ʸ����
    result << "(" << tail.cost/DECIM8(MULT) << EOT; // ʸ���Τγ�Ψ�ͤ�ɽ��
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
            cout << "�����ѿ� orig �� UD �Ǥ� line: " << __LINE__ << endl;
            exit(-1);
        default:
            cerr << "�����ѿ� orig �������Ǥ�: " << node->orig << endl;
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
