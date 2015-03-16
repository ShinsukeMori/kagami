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


//------------------------------------------------------------------------------------
//                       class DPNode
//------------------------------------------------------------------------------------

class DPNode;
typedef DPNode* DPNode_P;
typedef DPNode** DPNode_P_P;

class DPNode{

  public:

    U_INT4   stat;                                // �����ֹ�

    U_INT4   length;                              // Ĺ��

    U_INT4   text;                                // �Ѵ����ɽ��

    enum ORIGIN { UD /*̤��*/, IN /*��������*/, EX /*��������*/, IV /*����ñ��*/, 
                  UM /*̤�θ�*/};

    ORIGIN   orig;                                // �н�

    S_INT4   cost;                                // ���ѳ�Ψ

    DPNode_P prev;                                // ľ���ΥΡ���

    DPNode_P foll;                                // ľ��ΥΡ���

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
: stat(stat), length(0), text(0), orig(UD), cost(0), prev(NULL), foll(NULL)
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
    case DPNode::IV:
        return(string("IV"));
    case DPNode::UM:
        return(string("UM"));
    case DPNode::UD:
        return(string("UD"));
    default:
        cerr << "���� orig �������Ǥ�: " << endl;
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
    case DPNode::IV:
        cout << "IV";
        break;
    case DPNode::UM:
        cout << "UM";
        break;
    case DPNode::UD:
        cout << "UD";
        break;
    default:
        cerr << "���� orig �������Ǥ�: " << U_INT4(orig) << endl;
        exit(-1);
    }

    return(s);
}


//------------------------------------------------------------------------------------
//                       class VTable
//------------------------------------------------------------------------------------

// ��  ǽ : Viterbi ���르�ꥺ���ɽ
//
// ����� : ���������� DPNode_P_P vtable ������

class VTable{

  public:

                  VTable(const string&, const IntStr&, const Markov&, const IntStr&,
                         const Markov&, const UkWord&, U_INT4);

    void          init();                         // �����


    void          fill(/*const*/ InDict&, /*const*/ InDict&, const UkWord&, const U_INT4);
                                                  // ưŪ�ײ�ˡ��ɽ�ι���

    void          fill(/*const*/ InDict&, /*const*/ InDict&, /*const*/ ExDict&, const UkWord&,
                       const U_INT4);             // ưŪ�ײ�ˡ��ɽ�ι���

    void          output(W_CHAR_P, IntStr&, IntStr&); // ��̤ν���

    void          output(W_CHAR_P, IntStr&, IntStr&, IntStr&); // ��̤ν���

    string        result(W_CHAR_P, IntStr&, IntStr&); // ��̤�Ӽ����֤�

    string        result(W_CHAR_P, IntStr&, IntStr&, IntStr&); // ��̤�Ӽ����֤�

    void          fprint(ostream&, U_INT4) const; // ���󥹥��󥹤�ɽ��

  private:

    const IntStr& intstr;                         // ñ��ȿ������б�ɽ

    const Markov& markov;                         // �����ǥ�

    const IntStr& taskintstr;                     // ñ��ȿ������б�ɽ

    const Markov& taskmarkov;                     // �����ǥ�

    const UkWord& ukword;                         // ̤�θ��ǥ�

    S_INT4_P      TaskUWCost;                     // Mx(w) for w in only Task

    DECIM8        L1[3];                          // ft(w) > 0 ����ַ���

    DECIM8        L2[2];                          // ft(w) = 0 ����ַ���

    S_INT4        Cb1;                            // -mlog(L1[0])

    S_INT4        Cb2;                            // -mlog(L2[0])

    S_INT4        C11;                            // -mlog(L1[1])

    S_INT4        C21;                            // -mlog(L2[1])

    const U_INT4  maxlen;                         // Ĺ���κ�����

    typedef map<U_INT4, DPNode> DPColumn;         // �����ֹ� => �Ρ���
    typedef DPColumn* DPColumn_P;
    DPColumn_P    vtable;                         // ưŪ�ײ�ˡ��ɽ

    U_INT4        curpos;                         // ���ߤΰ���(ʸƬ�����ʸ����)

    void          Infill(/*const*/ InDict&, const U_INT4);// ɽ�ι���(��������)

    void          TaskInfill(/*const*/ InDict&, const U_INT4); // ɽ�ι���(����������)

    void          Exfill(/*const*/ ExDict&, const U_INT4);// ɽ�ι���(��������)

    void          Ukfill(const UkWord&, const U_INT4);// ɽ�ι���(̤�θ�)

    void          fill(const U_INT4&, const U_INT4&, const U_INT4&, const S_INT4&,
                       const DPNode::ORIGIN&, DPNode_P); // ɽ�ι���(������)

    void          postfill();

    S_INT4        logadd(S_INT4);
};


//------------------------------------------------------------------------------------
//                       constructor
//------------------------------------------------------------------------------------

VTable::VTable(const string& path, const IntStr& intstr, const Markov& markov, 
               const IntStr& taskintstr, const Markov& taskmarkov,
               const UkWord& ukword, U_INT4 maxlen)
: intstr(intstr), markov(markov), taskintstr(taskintstr), taskmarkov(taskmarkov), 
  ukword(ukword), maxlen(maxlen), curpos(0)
{
#ifdef VTable_DEBUG
    cerr << "VTable::VTable(const IntStr&, const Markov&, ..., U_INT4)" << endl;
#endif

    ifstream lambda((path + "TaskWordLambda").c_str());
    if (! lambda) openfailed(path + "TaskWordLambda");
    lambda >> L1[0] >> L1[1] >> L1[2];
    lambda >> L2[0] >> L2[1];
    cerr << "L1 = (" << L1[0] << ", " << L1[1] << ", " << L1[2] << "), "
         << "L2 = (" << L2[0] << ", " << L2[1] <<  ")" << endl;
    lambda.close();

    Cb1 = int(-MULT * log(L1[0]));
    Cb2 = int(-MULT * log(L2[0]));

    C11 = int(-MULT * log(L1[1]));
    C21 = int(-MULT * log(L2[1]));

    vtable = new DPColumn[maxlen];
    vtable[0][BT] = DPNode(BT);                   // ��ü������

    TaskUWCost = new S_INT4[taskintstr.size];     // -log(Mx(w)) for w in only Task
    for (U_INT4 stat = intstr.size; stat < taskintstr.size; stat++){
        WORD word = STR2WORD(taskintstr[stat]);
        TaskUWCost[stat] = ukword.cost(word);     // Mx(w)
    }
}


//------------------------------------------------------------------------------------
//                       init
//------------------------------------------------------------------------------------

void VTable::init()
{
#ifdef VTable_DEBUG
    cerr << "VTable::init()" << endl;
#endif

    for (U_INT4 i = 1; i <= curpos; i++){
        vtable[i].erase(vtable[i].begin(), vtable[i].end());
    }

    curpos = 0;
}


//------------------------------------------------------------------------------------
//                       fill
//------------------------------------------------------------------------------------

void VTable::fill(/*const*/ InDict& indict, /*const*/ InDict& taskindict,
                  const UkWord& ukword, U_INT4 fb = 0)
{
#ifdef VTable_DEBUG
    cerr << "VTable::fill(const InDict&, const InDict&, const UkWord&, ...)" << endl;
#endif

    curpos++;

    if (curpos < fb) return;                      // 1st boundary �ޤǤϥΡ��ɤ��餺

    Infill(indict, fb);
    TaskInfill(taskindict, fb);
    Ukfill(ukword, fb);

    postfill();
}


//------------------------------------------------------------------------------------

void VTable::fill(/*const*/ InDict& indict, /*const*/ InDict& taskindict,
                  /*const*/ ExDict& exdict, const UkWord& ukword, U_INT4 fb = 0)
{
#ifdef VTable_DEBUG
    cerr << "VTable::fill(const InDict&, const ExDict&, const UkWord&)" << endl;
#endif

    curpos++;

    if (curpos < fb) return;                      // 1st boundary �ޤǤϥΡ��ɤ��餺

    Infill(indict, fb);
    TaskInfill(taskindict, fb);
    Exfill(exdict, fb);
    Ukfill(ukword, fb);

    postfill();
}


//------------------------------------------------------------------------------------

// ��  ǽ : Viterbi ���르�ꥺ���ɽ vtable[curpos] ����������ˤ�������Ǥ�����
//
// ����� : �ʤ�

inline void VTable::Infill(/*const*/ InDict& indict, U_INT4 fb = 0)
{
#ifdef VTable_DEBUG
    cerr << "VTable::Infill(const InDict&, U_INT4 = 0)" << endl;
#endif

    for (InMorp_P morp = indict.lenpos(); morp->length > 0; morp++){
        if ((curpos-morp->length < fb) && (fb < curpos)) continue;
                                                  // fb ��٤��Ρ��ɤ��������ʤ�
        for (DPColumn::iterator iter = vtable[curpos-morp->length].begin();
             iter != vtable[curpos-morp->length].end(); iter++){
            DPNode_P prev = &((*iter).second);
            S_INT4 cost = prev->cost;             // ���ξ��֤Υ�����
            U_INT4 prevstat = (prev->stat < intstr.size) ? prev->stat : UT;

            if (taskmarkov._1cost(prev->stat) != S_INT4_MAX-1){ // Ft(w0) > 0
                S_INT4 CM = markov.cost(prevstat, morp->stat);
                S_INT4 Ct = taskmarkov.cost(prev->stat, morp->stat);
                cost += Cb1+CM-logadd(Ct-Cb1-CM);
            }else{                                              // Ft(w0) == 0
                S_INT4 CM = markov.cost(prevstat, morp->stat);
                S_INT4 Ct = taskmarkov._1cost(morp->stat)-C11+C21; // Cost/L1[1]*L2[1]
                cost += Cb2+CM-logadd(Ct-Cb2-CM);
            }
            cost += morp->cost;                   // -log P(y|x)

            fill(morp->stat, morp->length, morp->stat, cost, DPNode::IN, prev);
        }
    }
}


//------------------------------------------------------------------------------------

// ��  ǽ : Viterbi ���르�ꥺ���ɽ vtable[curpos] ����������ˤ�������Ǥ�����
//
// ����� : �ʤ�

inline void VTable::TaskInfill(/*const*/ InDict& indict, U_INT4 fb = 0)
{
#ifdef VTable_DEBUG
    cerr << "VTable::TaskInfill(const InDict&, U_INT4 = 0)" << endl;
#endif

    for (InMorp_P morp = indict.lenpos(); morp->length > 0; morp++){
        if ((curpos-morp->length < fb) && (fb < curpos)) continue;
                                                  // fb ��٤��Ρ��ɤ��������ʤ�
        for (DPColumn::iterator iter = vtable[curpos-morp->length].begin();
             iter != vtable[curpos-morp->length].end(); iter++){
            DPNode_P prev = &((*iter).second);
            S_INT4 cost = prev->cost;             // ���ξ��֤Υ�����
            U_INT4 prevstat = (prev->stat < intstr.size) ? prev->stat : UT;

            if (taskmarkov._1cost(prev->stat) == S_INT4_MAX-1){ // Ft(w0) > 0
                S_INT4 CM = markov.cost(prevstat, UT)+TaskUWCost[morp->stat];
                S_INT4 Ct = taskmarkov.cost(prev->stat, morp->stat);
                cost += Cb1+CM-logadd(Ct-Cb1-CM);
            }else{                                              // Ft(w0) == 0
                S_INT4 CM = markov.cost(prevstat, UT)+TaskUWCost[morp->stat];
                S_INT4 Ct = taskmarkov._1cost(morp->stat)-C11+C21; // Cost/L1[1]*L2[1]
                cost += Cb2+CM-logadd(Ct-Cb2-CM);
            }
            cost += morp->cost;                   // -log P(y|x)

            fill(morp->stat, morp->length, morp->stat, cost, DPNode::IV, prev);
        }
    }
}


//------------------------------------------------------------------------------------

// ��  ǽ : Viterbi ���르�ꥺ���ɽ vtable[curpos] ��������ˤ�������Ǥ�����
//
// ����� : �ʤ�

inline void VTable::Exfill(/*const*/ ExDict& exdict, U_INT4 fb = 0)
{
#ifdef VTable_DEBUG
    cerr << "VTable::Exfill(const ExDict&, U_INT4 = 0)" << endl;
#endif

    for (ExMorp_P morp = exdict.lenpos(); morp->length > 0; morp++){
        if ((curpos-morp->length < fb) && (fb < curpos)) continue;
                                                  // fb ��٤��Ρ��ɤ��������ʤ�
        for (DPColumn::iterator iter = vtable[curpos-morp->length].begin();
             iter != vtable[curpos-morp->length].end(); iter++){
            DPNode_P prev = &((*iter).second);
            S_INT4 cost = prev->cost;             // ���ξ��֤Υ�����
            U_INT4 prevstat = (prev->stat < intstr.size) ? prev->stat : UT;

            if (taskmarkov._1cost(prev->stat) == S_INT4_MAX-1){ // Ft(w0) > 0
                S_INT4 CM = markov.cost(prevstat, UT);
                S_INT4 Ct = taskmarkov.cost(prev->stat, UT);
                cost += Cb1+CM-logadd(Ct-Cb1-CM);
            }else{                                              // Ft(w0) == 0
                S_INT4 CM = markov.cost(prevstat, UT);
                S_INT4 Ct = taskmarkov._1cost(UT)-C11+C21; // Cost/L1[1]*L2[1]
                cost += Cb2+CM-logadd(Ct-Cb2-CM);
            }
            cost += morp->cost;                   // -log P(y|x)

            fill(UT, morp->length, morp->text, cost, DPNode::IN, prev);
        }
    }
}


//------------------------------------------------------------------------------------

// ��  ǽ : Viterbi ���르�ꥺ���ɽ vtable[curpos] ��̤�η����Ǥ�����
//
// ����� : �ʤ�

inline void VTable::Ukfill(const UkWord& ukword, U_INT4 fb = 0)
{
#ifdef VTable_DEBUG
    cerr << "VTable::Ukfill(UkWord&)" << endl;
#endif

#ifdef _StopChar_h
    for (U_INT4 length = ukword.lmax(); length > 0; length--){
#else
    for (U_INT4 length = min(curpos, UkWordMaxLen); length > 0; length--){
#endif
        if ((curpos-length < fb) && (fb < curpos)) continue;
                                                  // fb ��٤��Ρ��ɤ��������ʤ�
        for (DPColumn::iterator iter = vtable[curpos-length].begin();
             iter != vtable[curpos-length].end(); iter++){
            DPNode_P prev = &((*iter).second);
            S_INT4 cost = prev->cost;             // ���ξ��֤Υ�����
            U_INT4 prevstat = (prev->stat < intstr.size) ? prev->stat : UT;

            if (taskmarkov._1cost(prev->stat) == S_INT4_MAX-1){ // Ft(w0) > 0
                S_INT4 CM = markov.cost(prevstat, UT);
                S_INT4 Ct = taskmarkov.cost(prev->stat, UT);
                cost += Cb1+CM-logadd(Ct-Cb1-CM);
            }else{                                              // Ft(w0) == 0
                S_INT4 CM = markov.cost(prevstat, UT);
                S_INT4 Ct = taskmarkov._1cost(UT)-C11+C21; // Cost/L1[1]*L2[1]
                cost += Cb2+CM-logadd(Ct-Cb2-CM);
            }
            cost += ukword.cost(length);                   // -log P(y|x) ??

            fill(UT, length, 0, cost, DPNode::UM, prev);
        }
    }
}

//------------------------------------------------------------------------------------

// ��  ǽ : Viterbi ���르�ꥺ���ɽ vtable[curpos] ������� (stat, length) ������
//
// ����� : �ʤ�

inline void VTable::fill(const U_INT4& stat, const U_INT4& length,
                         const U_INT4& text, const S_INT4& Cost,
                         const DPNode::ORIGIN& orig, DPNode_P prev)
{
#ifdef VTABLE_DEBUG
    cerr << "VTable::fill(const U_INT4&, const U_INT4&, const DECIM8&, "
         << "const DPNode::ORIGIN&)" << endl;
#endif

//    S_INT4 cost = prev->cost+Cost;
    S_INT4 cost = Cost;
    if ((vtable[curpos].find(stat) == vtable[curpos].end()) ||
            (vtable[curpos][stat].cost > cost)){
        vtable[curpos][stat] = DPNode(stat, length, text, orig, cost, prev);
    }
}


//------------------------------------------------------------------------------------
//                       postfill
//------------------------------------------------------------------------------------

// ��  ǽ : �����ѿ� head[curpos] �ȥ����ѿ� next ������
//
// ����� : �ʤ�

inline void VTable::postfill()
{
#ifdef VTable_DEBUG
    cerr << "VTable::postfill()" << endl;
#endif

    // NOP
}


//------------------------------------------------------------------------------------
//                       logadd
//------------------------------------------------------------------------------------

// ��  ǽ : m*log(1+exp(-x/m)), where m = MULT
//
// ����� : ������ơ��֥���ѹ�

inline S_INT4 VTable::logadd(S_INT4 x)
{
#ifdef VTable_DEBUG
    cerr << "VTable::logadd(S_INT4)" << endl;
#endif

//    return(0);
    return(int(MULT*log(1+exp(-DECIM8(x)/MULT))));
}


//------------------------------------------------------------------------------------
//                       output
//------------------------------------------------------------------------------------

void VTable::output(W_CHAR_P sent, IntStr& intstr, IntStr& taskintstr)
{
#ifdef DEBUG
    cerr << "VTable::output(W_CHAR_P, IntStr&)" << endl;
#endif

    DPNode tail;                                  // ��ü�ΥΡ���
    for (DPColumn::iterator iter = vtable[curpos].begin();
         iter != vtable[curpos].end(); iter++){

        DPNode_P prev = &((*iter).second);
        S_INT4 cost = prev->cost;                 // ���ξ��֤Υ�����
        U_INT4 prevstat = (prev->stat < intstr.size) ? prev->stat : UT;

        if (taskmarkov._1cost(prev->stat) == S_INT4_MAX-1){ // Ft(w0) > 0
            S_INT4 CM = markov.cost(prevstat, BT);
            S_INT4 Ct = taskmarkov.cost(prev->stat, BT);
            cost += Cb1+CM-logadd(Ct-Cb1-CM);
        }else{                                              // Ft(w0) == 0
            S_INT4 CM = markov.cost(prevstat, BT);
            S_INT4 Ct = taskmarkov._1cost(BT)-C11+C21; // Cost/L1[1]*L2[1]
            cost += Cb2+CM-logadd(Ct-Cb2-CM);
        }

        if ((tail.stat == UT) || (tail.cost > cost)){
            tail = DPNode(BT, 1, 0, DPNode::IN, cost, prev);
        }
    }
    assert(tail.stat == BT);                    // �򤬤ʤ�
    curpos++;

// �����õ���ȥ��� foll ������
    DPNode_P node;
    for (node = &tail; node->prev != NULL; node = node->prev){
        node->prev->foll = node;
    }

// ������õ���Ⱥ�����ɽ��
    for (node = node->foll; node->foll != NULL; node = node->foll){
        switch (node->orig){
        case DPNode::IN:
            cout << intstr[node->text];
            break;
        case DPNode::IV:
            cout << taskintstr[node->text];
            break;
        case DPNode::EX:
            cerr << "�����ѿ� orig �������Ǥ�: " << node->orig << endl;
            exit(-1);
        case DPNode::UM:
            cout.write((S_CHAR_P)sent, node->length*2);
            break;
        default:
            cerr << "�����ѿ� orig �������Ǥ�: " << node->orig << endl;
            exit(-1);
        }
        cout << "/" << node->orig << " ";
        sent += node->length;
    }

    cout << tail.cost/log(2)/MULT << endl;// ʸ���Τγ�Ψ�ͤ�ɽ��
}


//------------------------------------------------------------------------------------

void VTable::output(W_CHAR_P sent, IntStr& intstr, IntStr& taskintstr, IntStr& extext)
{
#ifdef DEBUG
    cerr << "VTable::output(W_CHAR_P, IntStr&)" << endl;
#endif

    DPNode tail;                                  // ��ü�ΥΡ���
    for (DPColumn::iterator iter = vtable[curpos].begin();
         iter != vtable[curpos].end(); iter++){

        DPNode_P prev = &((*iter).second);
        S_INT4 cost = prev->cost;                 // ���ξ��֤Υ�����
        U_INT4 prevstat = (prev->stat < intstr.size) ? prev->stat : UT;

        if (taskmarkov._1cost(prev->stat) == S_INT4_MAX-1){ // Ft(w0) > 0
            S_INT4 CM = markov.cost(prevstat, BT);
            S_INT4 Ct = taskmarkov.cost(prev->stat, BT);
            cost += Cb1+CM-logadd(Ct-Cb1-CM);
        }else{                                              // Ft(w0) == 0
            S_INT4 CM = markov.cost(prevstat, BT);
            S_INT4 Ct = taskmarkov._1cost(BT)-C11+C21; // Cost/L1[1]*L2[1]
            cost += Cb2+CM-logadd(Ct-Cb2-CM);
        }

        if ((tail.stat == UT) || (tail.cost > cost)){
            tail = DPNode(BT, 1, 0, DPNode::IN, cost, prev);
        }
    }
    assert(tail.stat == BT);                    // �򤬤ʤ�
    curpos++;

// �����õ���ȥ��� foll ������
    DPNode_P node;
    for (node = &tail; node->prev != NULL; node = node->prev){
        node->prev->foll = node;
    }

// ������õ���Ⱥ�����ɽ��
    for (node = node->foll; node->foll != NULL; node = node->foll){
        switch (node->orig){
        case DPNode::IN:
            cout << intstr[node->text];
            break;
        case DPNode::IV:
            cout << taskintstr[node->text];
            break;
        case DPNode::EX:
            cerr << "�����ѿ� orig �������Ǥ�: " << node->orig << endl;
            cout << extext[node->text];
            break;
        case DPNode::UM:
            cout.write((S_CHAR_P)sent, node->length*2);
            break;
        default:
            cerr << "�����ѿ� orig �������Ǥ�: " << node->orig << endl;
            exit(-1);
        }
        cout << "/" << node->orig << " ";
        sent += node->length;
    }

    cout << tail.cost/log(2)/MULT << endl;// ʸ���Τγ�Ψ�ͤ�ɽ��
}


//------------------------------------------------------------------------------------
//                       result
//------------------------------------------------------------------------------------

string VTable::result(W_CHAR_P sent, IntStr& intstr, IntStr& taskintstr)
{
#ifdef DEBUG
    cerr << "VTable::output(W_CHAR_P, IntStr&)" << endl;
#endif

    DPNode tail;                                  // ��ü�ΥΡ���
    for (DPColumn::iterator iter = vtable[curpos].begin();
         iter != vtable[curpos].end(); iter++){

        DPNode_P prev = &((*iter).second);
        S_INT4 cost = prev->cost;                 // ���ξ��֤Υ�����
        U_INT4 prevstat = (prev->stat < intstr.size) ? prev->stat : UT;

        if (taskmarkov._1cost(prev->stat) == S_INT4_MAX-1){ // Ft(w0) > 0
            S_INT4 CM = markov.cost(prevstat, BT);
            S_INT4 Ct = taskmarkov.cost(prev->stat, BT);
            cost += Cb1+CM-logadd(Ct-Cb1-CM);
        }else{                                              // Ft(w0) == 0
            S_INT4 CM = markov.cost(prevstat, BT);
            S_INT4 Ct = taskmarkov._1cost(BT)-C11+C21; // Cost/L1[1]*L2[1]
            cost += Cb2+CM-logadd(Ct-Cb2-CM);
        }

        if ((tail.stat == UT) || (tail.cost > cost)){
            tail = DPNode(BT, 1, 0, DPNode::IN, cost, prev);
        }
    }
    assert(tail.stat == BT);                    // �򤬤ʤ�
    curpos++;

// �����õ���ȥ��� foll ������
    DPNode_P node;
    for (node = &tail; node->prev != NULL; node = node->prev){
        node->prev->foll = node;
    }

// ������õ���Ⱥ�����ɽ��
    ostringstream result;                         // ���Ϥ���ʸ����
    result << "(" << tail.cost/log(2)/MULT << EOT;// ʸ���Τγ�Ψ�ͤ�ɽ��
    for (node = node->foll; node->foll != NULL; node = node->foll){
        result << "(";
        switch (node->orig){
        case DPNode::IN:
            result << intstr[node->stat];
            break;
        case DPNode::IV:
            result << taskintstr[node->stat];
            break;
        case DPNode::EX:
            cerr << "�����ѿ� orig �������Ǥ�: " << node->orig << endl;
            break;
        case DPNode::UM:
            for (U_INT4 i = 0; i < node->length; i++){
                result << sent[i].half.hi << sent[i].half.lo;
            }
            break;
        default:
            cerr << "�����ѿ� orig �������Ǥ�: " << node->orig << endl;
            exit(-1);
        }
        result << " ";
        for (U_INT4 i = 0; i < node->length; i++){
            result << sent[i].half.hi << sent[i].half.lo;
        }
        result << " " << node->origstring() << ")"; 
        sent += node->length;
    }
    result << ")"; 

    return(result.str());
}


//------------------------------------------------------------------------------------

string VTable::result(W_CHAR_P sent, IntStr& intstr, IntStr& taskintstr, IntStr& extext)
{
#ifdef DEBUG
    cerr << "VTable::output(W_CHAR_P, IntStr&)" << endl;
#endif

    DPNode tail;                                  // ��ü�ΥΡ���
    for (DPColumn::iterator iter = vtable[curpos].begin();
         iter != vtable[curpos].end(); iter++){

        DPNode_P prev = &((*iter).second);
        S_INT4 cost = prev->cost;                 // ���ξ��֤Υ�����
        U_INT4 prevstat = (prev->stat < intstr.size) ? prev->stat : UT;

        if (taskmarkov._1cost(prev->stat) == S_INT4_MAX-1){ // Ft(w0) > 0
            S_INT4 CM = markov.cost(prevstat, BT);
            S_INT4 Ct = taskmarkov.cost(prev->stat, BT);
            cost += Cb1+CM-logadd(Ct-Cb1-CM);
        }else{                                              // Ft(w0) == 0
            S_INT4 CM = markov.cost(prevstat, BT);
            S_INT4 Ct = taskmarkov._1cost(BT)-C11+C21; // Cost/L1[1]*L2[1]
            cost += Cb2+CM-logadd(Ct-Cb2-CM);
        }

        if ((tail.stat == UT) || (tail.cost > cost)){
            tail = DPNode(BT, 1, 0, DPNode::IN, cost, prev);
        }
    }
    assert(tail.stat == BT);                      // �򤬤ʤ�
    curpos++;

// �����õ���ȥ��� foll ������
    DPNode_P node;
    for (node = &tail; node->prev != NULL; node = node->prev){
        node->prev->foll = node;
    }

// ������õ���Ⱥ�����ɽ��
    ostringstream result;                         // ���Ϥ���ʸ����
    result << "(" << tail.cost/log(2)/MULT << EOT;// ʸ���Τγ�Ψ�ͤ�ɽ��
    for (node = node->foll; node->foll != NULL; node = node->foll){
        result << "(";
        switch (node->orig){
        case DPNode::IN:
            result << intstr[node->stat];
            break;
        case DPNode::IV:
            result << taskintstr[node->stat];
            break;
        case DPNode::EX:
            result << extext[node->stat];
            break;
        case DPNode::UM:
            for (U_INT4 i = 0; i < node->length; i++){
                result << sent[i].half.hi << sent[i].half.lo;
            }
            break;
        default:
            cerr << "�����ѿ� orig �������Ǥ�: " << node->orig << endl;
            exit(-1);
        }
        result << EOT;
        for (U_INT4 i = 0; i < node->length; i++){ // ����ʸ����
            result << sent[i].half.hi << sent[i].half.lo;
        }
        result << EOT << node->origstring() << ")"; 
        sent += node->length;
    }
    result << ")"; 

    return(result.str());
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
