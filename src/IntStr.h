//====================================================================================
//                       IntStr.h
//                            by Shinsuke MORI
//                            Last change : 13 MIntStrh 1996
//====================================================================================

// ��  ǽ : 
//
// ����� : 


//------------------------------------------------------------------------------------
//                       define
//------------------------------------------------------------------------------------

#ifndef _IntStr_h
#define _IntStr_h

#ifdef DEBUG
#define IntStr_DEBUG
#endif
//#define IntStr_DEBUG


//------------------------------------------------------------------------------------
//                       include
//------------------------------------------------------------------------------------

#include <mystd.h>

#include <Word.h>


//------------------------------------------------------------------------------------
//                       class IntStr
//------------------------------------------------------------------------------------

class IntStr
{

  public:

    U_INT4          size;                         // �ǡ����ο�

                    IntStr();

                    IntStr(const string&, const string&);

    const void      readfile(const string&, const string&);

    const S_CHAR_P  operator[](const S_INT4) const; // ʸ����ؤ��Ѵ�

    const S_INT4    operator[] (const WORD) const; // �����ؤ��Ѵ�

    const S_INT4    operator[] (const string&) const; // �����ؤ��Ѵ�

    friend ostream& operator<<(ostream& s, const IntStr& intstr);

  protected:

    S_CHAR_P        data;                         // (S_CHAR[]-\0)+

    U_INT4_P        list;                         // data ��ź��������

};


//------------------------------------------------------------------------------------
//                       constructor
//------------------------------------------------------------------------------------

IntStr::IntStr()
{
    size = 0;
    data = NULL;
    list = NULL;
}

IntStr::IntStr(const string& filestem, const string& suffix = ".text")
{
    const string filename = filestem+suffix;

    ifstream file(filename.c_str());
    if (! file) openfailed(filename);

    file.seekg(0, ios::end);                      // �ե����륵�������������
    size = (U_INT4)file.tellg();                  // ����� size �ΰ�̣�ǤϤʤ�
    file.seekg(0, ios::beg);

    data = new S_CHAR[size];                      // �ǡ����������Ƥ�������γ���
    cerr << memalloced(size*sizeof(S_CHAR)) << " for " << filename << endl;

    size = 0;
    for (S_CHAR_P temp = data; file.read(temp, sizeof(S_CHAR)); temp++){
        if (*temp == '\n') *temp = EOS, size++;
    }

    list = new U_INT4[size];                      // ź���򵭲����Ƥ�������γ���
    cerr << memalloced(size*sizeof(U_INT4)) << " for " << filename << endl;

    for (U_INT4 pos = 0, suf = 0; suf < size; suf++, pos++){
        list[suf] = pos;
        while (data[pos] != EOS) pos++;
    }
}


//------------------------------------------------------------------------------------
//                       readfile
//------------------------------------------------------------------------------------

const void IntStr::readfile(const string& filestem, const string& suffix = ".text")
{
#ifdef MorpIntStr_DEBUG
    cerr << "MorpIntStr::readfile(string)" << endl;
#endif

    const string filename = filestem+suffix;

    ifstream file(filename.c_str());
    if (! file) openfailed(filename);

    file.seekg(0, ios::end);                      // �ե����륵�������������
    size = (U_INT4)file.tellg();                  // ����� size �ΰ�̣�ǤϤʤ�
    file.seekg(0, ios::beg);

    data = new S_CHAR[size];                      // �ǡ����������Ƥ�������γ���
    cerr << memalloced(size*sizeof(S_CHAR)) << " for " << filename << endl;

    size = 0;
    for (S_CHAR_P temp = data; file.read(temp, sizeof(S_CHAR)); temp++){
        if (*temp == '\n') *temp = EOS, size++;
    }

    list = new U_INT4[size];                      // ź���򵭲����Ƥ�������γ���
    cerr << memalloced(size*sizeof(U_INT4)) << " for " << filename << endl;

    for (U_INT4 pos = 0, suf = 0; suf < size; suf++, pos++){
        list[suf] = pos;
        while (data[pos] != EOS) pos++;
    }
}


//------------------------------------------------------------------------------------
//                       operator[]
//------------------------------------------------------------------------------------

inline const S_CHAR_P IntStr::operator[] (const S_INT4 suf) const
{
#ifdef IntStr_DEBUG
    cerr << "IntStr::operator[](const S_INT4)" << endl;
#endif

    return(data+list[suf]);
}


//------------------------------------------------------------------------------------
//                       operator[]
//------------------------------------------------------------------------------------

inline const S_INT4 IntStr::operator[] (const WORD word) const
{
#ifdef IntStr_DEBUG
    cerr << "IntStr::operator[](const WORD)" << endl;
#endif
    
//    if (word == STR2WORD("BT")){
    if (word == BTword){
        return(1);
    }
    
    U_INT4 gauche = 1;                            // ��ü
    U_INT4 droite = size;                         // ��ü
    U_INT4 centre;                                // �濴

    while (gauche+1 < droite){                    // ��ʬõ���Υ롼��
//        cerr << "(gauche, droite) = (" << gauche << ", " << droite << ")" << endl;
        centre = (gauche+droite)/2;

        if (word < STR2WORD(data+list[centre])){  // word < string[centre]
//            cerr << word << " < " << data+list[centre] << endl;
            droite = centre;
            continue;
        }
        if (word > STR2WORD(data+list[centre])){  // word > string[centre]
//            cerr << word << " > " << data+list[centre] << endl;
            gauche = centre;
            continue;
        }
//        cerr << word << " = " << data+list[centre] << endl;
        return(centre);                           // word = substr
    }

    return(0);                                    // ���Ĥ��餺
}


//------------------------------------------------------------------------------------
//                       operator[]
//------------------------------------------------------------------------------------

inline const S_INT4 IntStr::operator[] (const string& word) const
{
#ifdef IntStr_DEBUG
    cerr << "IntStr::operator[](const string&)" << endl;
#endif
    
    if (word == string("BT")){
        return(1);
    }
    
    U_INT4 gauche = 1;                            // ��ü
    U_INT4 droite = size;                         // ��ü
    U_INT4 centre;                                // �濴

    while (gauche+1 < droite){                    // ��ʬõ���Υ롼��
//        cerr << "(gauche, droite) = (" << gauche << ", " << droite << ")" << endl;
        centre = (gauche+droite)/2;

        if (word < data+list[centre]){  // word < string[centre]
//            cerr << word << " < " << data+list[centre] << endl;
            droite = centre;
            continue;
        }
        if (word > data+list[centre]){  // word > string[centre]
//            cerr << word << " > " << data+list[centre] << endl;
            gauche = centre;
            continue;
        }
//        cerr << word << " = " << data+list[centre] << endl;
        return(centre);                           // word = substr
    }

    return(0);                                    // ���Ĥ��餺
}


//------------------------------------------------------------------------------------
//                       operator<<
//------------------------------------------------------------------------------------

inline ostream& operator<<(ostream& s, const IntStr& intstr)
{
    for (U_INT4 i = 0; i < intstr.size; i++){
        s << setw(4) << i << " " << intstr.data+intstr.list[i] << endl;
    }
    return s;
}


//------------------------------------------------------------------------------------
//                       endif
//------------------------------------------------------------------------------------

#endif


//====================================================================================
//                       END
//====================================================================================
