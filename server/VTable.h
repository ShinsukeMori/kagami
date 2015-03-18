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

#include <math.h>
#include <mystd.h>
#include <minmax.h>

#include "UkWord.h"
#include "Markov.h"
#include "IntStr.h"
#include "InMorp.h"
#include "ExMorp.h"
#include "DPNode.h"


//------------------------------------------------------------------------------------
//                       class VTable
//------------------------------------------------------------------------------------

// ��  ǽ : Viterbi ���르�ꥺ���ɽ
//
// ����� : ���������� DPNode_P_P vtable ������

class VTable{

  public:

                  VTable(const IntStr&, const Markov&, U_INT4);

    void          init();                         // �����

    void          fill(const InDict&, const UkWord&);
                                                  // ưŪ�ײ�ˡ��ɽ�ι���

    void          fill(const InDict&, const ExDict&, const UkWord&);
                                                  // ưŪ�ײ�ˡ��ɽ�ι���

    void          output(W_CHAR_P, IntStr&);      // ��̤ν���

    void          output(W_CHAR_P, IntStr&, IntStr&);
                                                  // ��̤ν���

    void          fprint(ostream&, U_INT4) const; // ���󥹥��󥹤�ɽ��

  private:

    const IntStr& intstr;                         // �����Ǥȿ������б�ɽ

    const Markov& markov;                         // �����ǥ�

    const U_INT4  maxlen;                         // Ĺ���κ�����

    typedef map<U_INT4, DPNode> DPColumn;
    typedef DPColumn* DPColumn_P;

    DPColumn_P    vtable;                         // ưŪ�ײ�ˡ��ɽ

    U_INT4        curpos;                         // ���ߤΰ���(ʸƬ�����ʸ����)

    void          fill(const InDict&);            // ưŪ�ײ�ˡ��ɽ�ι���(��������)

    void          fill(const ExDict&);            // ưŪ�ײ�ˡ��ɽ�ι���(��������)

    void          fill(const UkWord&);            // ưŪ�ײ�ˡ��ɽ�ι���(̤�η�����)

    void          fill(const U_INT4&, const U_INT4&, const U_INT4&, const S_INT4&,
                       const DPNode::ORIGIN&);    // ưŪ�ײ�ˡ��ɽ�ι���(������)

};


//------------------------------------------------------------------------------------
//                       constructor
//------------------------------------------------------------------------------------

VTable::VTable(const IntStr& intstr, const Markov& markov, U_INT4 maxlen)
: intstr(intstr), markov(markov), maxlen(maxlen), curpos(0)
{
#ifdef VTable_DEBUG
    cerr << "VTable::VTable(const IntStr&, const Markov&, U_INT4)" << endl;
#endif

    vtable = new DPColumn[maxlen];
    vtable[0][BT] = DPNode(BT);
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

void VTable::fill(const InDict& indict, const UkWord& ukword)
{
#ifdef VTable_DEBUG
    cerr << "VTable::fill(const InDict&, const UkWord&)" << endl;
#endif

    curpos++;
    fill(indict);
    fill(ukword);
}


//------------------------------------------------------------------------------------

void VTable::fill(const InDict& indict, const ExDict& exdict, const UkWord& ukword)
{
#ifdef VTable_DEBUG
    cerr << "VTable::fill(const InDict&, const ExDict&, const UkWord&)" << endl;
#endif

    curpos++;
    fill(indict);
    fill(exdict);
    fill(ukword);
}


//------------------------------------------------------------------------------------

// ��  ǽ : Viterbi ���르�ꥺ���ɽ vtable[curpos] ����������ˤ�������Ǥ�����
//
// ����� : �ʤ�

inline void VTable::fill(const InDict& indict)
{
#ifdef VTable_DEBUG
    cerr << "VTable::fill(const InDict&)" << endl;
#endif

    for (InMorp_P morp = indict.lenpos(); morp->length > 0; morp++){
        fill(morp->stat, morp->length, morp->text, morp->cost, DPNode::IN);
    }
}


//------------------------------------------------------------------------------------

// ��  ǽ : Viterbi ���르�ꥺ���ɽ vtable[curpos] ��������ˤ�������Ǥ�����
//
// ����� : �ʤ�

inline void VTable::fill(const ExDict& exdict)
{
#ifdef VTable_DEBUG
    cerr << "VTable::fill(const ExDict&)" << endl;
#endif

    for (ExMorp_P morp = exdict.lenpos(); morp->length > 0; morp++){
        fill(UT, morp->length, morp->text, morp->cost, DPNode::EX);
    }
}


//------------------------------------------------------------------------------------

// ��  ǽ : Viterbi ���르�ꥺ���ɽ vtable[curpos] ��̤�η����Ǥ�����
//
// ����� : �ʤ�

inline void VTable::fill(const UkWord& ukword)
{
#ifdef VTable_DEBUG
    cerr << "VTable::fill(UkWord&)" << endl;
#endif

#ifdef _StopChar_h
    for (U_INT4 length = ukword.lmax(); length > 0; length--){
#else
    for (U_INT4 length = min(curpos, UkWordMaxLen); length > 0; length--){
#endif
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
#ifdef VTABLE_DEBUG
    cerr << "VTable::fill(const U_INT4&, const U_INT4&, const S_INT4&, "
         << "const DPNode::ORIGIN&)" << endl;
#endif

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
//                       output
//------------------------------------------------------------------------------------

void VTable::output(W_CHAR_P sent, IntStr& intext)
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

// ������õ���Ⱥ�����ɽ��
    for (node = node->foll; node->foll != NULL; node = node->foll){
        switch (node->orig){
        case DPNode::IN:
            cout << intstr[node->text];
            break;
        case DPNode::EX:
            cerr << "�����ѿ� orig = EX �������Ǥ� line: " << __LINE__ << endl;
            exit(-1);
        case DPNode::UW:
            cout.write((S_CHAR_P)sent, node->length*2);
            break;
        case DPNode::UD:
            cout << "�����ѿ� orig �� UD �Ǥ� line: " << __LINE__ << endl;
            exit(-1);
        default:
            cerr << "�����ѿ� orig �������Ǥ�: " << node->orig << endl;
            exit(-1);
        }
        cout << "/" << node->orig << " ";
        sent += node->length;
    }

//    cout.precision(5);
//    cout << tail->cost; // ʸ���Τγ�Ψ�ͤ�ɽ��

    cout << endl;                              
}


//------------------------------------------------------------------------------------

void VTable::output(W_CHAR_P sent, IntStr& intstr, IntStr& exword)
{
#ifdef VTable_DEBUG
    cerr << "VTable::output(W_CHAR_P, IntStr&, IntStr&)" << endl;
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

// ������õ���Ⱥ�����ɽ��
    for (node = node->foll; node->foll != NULL; node = node->foll){
        switch (node->orig){
        case DPNode::IN:
            cout << intstr[node->text];
            break;
        case DPNode::EX:
            cout << exword[node->text];
            break;
        case DPNode::UW:
            cout.write((S_CHAR_P)sent, node->length*2);
            break;
        default:
            cerr << "�����ѿ� orig �������Ǥ�: " << node->orig << endl;
            exit(-1);
        }
        cout << "/" << node->orig << " ";
        sent += node->length;
    }

//    cout.precision(5);
//    cout << tail->cost; // ʸ���Τγ�Ψ�ͤ�ɽ��

    cout << endl;                              
}


//------------------------------------------------------------------------------------
//                       fprint
//------------------------------------------------------------------------------------

#ifdef VTable_DEBUG
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

    for (U_INT4 suf1 = 0; suf1 < intstr.size; suf1++){
        fout << setw(2) << suf1 << " ";
        for (U_INT4 suf2 = 0; suf2 < maxpos; suf2 += 2){
            fout << vtable[suf2][suf1];
        }
        fout << endl;
    }
}
#endif


//------------------------------------------------------------------------------------
//                       endif
//------------------------------------------------------------------------------------

#endif


//====================================================================================
//                       END
//====================================================================================
