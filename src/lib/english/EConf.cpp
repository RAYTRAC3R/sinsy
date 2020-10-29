/* ----------------------------------------------------------------- */
/*           The HMM-Based Singing Voice Synthesis System "Sinsy"    */
/*           developed by Sinsy Working Group                        */
/*           http://sinsy.sourceforge.net/                           */
/* ----------------------------------------------------------------- */
/*                                                                   */
/*  Copyright (c) 2009-2015  Nagoya Institute of Technology          */
/*                           Department of Computer Science          */
/*  Copyright (C) 2017-2018 HyperZLink (a.k.a hyperzlib / Quantum)   */
/*                                                                   */
/* All rights reserved.                                              */
/*                                                                   */
/* Redistribution and use in source and binary forms, with or        */
/* without modification, are permitted provided that the following   */
/* conditions are met:                                               */
/*                                                                   */
/* - Redistributions of source code must retain the above copyright  */
/*   notice, this list of conditions and the following disclaimer.   */
/* - Redistributions in binary form must reproduce the above         */
/*   copyright notice, this list of conditions and the following     */
/*   disclaimer in the documentation and/or other materials provided */
/*   with the distribution.                                          */
/* - Neither the name of the Sinsy working group nor the names of    */
/*   its contributors may be used to endorse or promote products     */
/*   derived from this software without specific prior written       */
/*   permission.                                                     */
/*                                                                   */
/* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND            */
/* CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,       */
/* INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF          */
/* MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE          */
/* DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS */
/* BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,          */
/* EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED   */
/* TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,     */
/* DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON */
/* ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,   */
/* OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY    */
/* OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE           */
/* POSSIBILITY OF SUCH DAMAGE.                                       */
/* ----------------------------------------------------------------- */

#include <iostream>
#include <stdexcept>
#include <limits>
#include <queue>
#include <memory>
#include <vector>
#include <iterator>
#include "util_log.h"
#include "util_string.h"
#include "util_converter.h"
#include "StringTokenizer.h"
#include "EConf.h"
#include "Deleter.h"

namespace sinsy
{

namespace
{
const std::string SIL_STR = "sil";
const std::string SEPARATOR = ",";
const std::string LANGUAGE_INFO = "ENG";
const std::string MACRON = "MACRON";
const std::string SYLLABLE_DELIMITER = "SYLLABLE_DELIMITER";
const std::string VOWELS = "VOWELS";
const std::string CONSONANTS = "CONSONANTS";
const std::string MULTIBYTE_CHAR_RANGE = "MULTIBYTE_CHAR_RANGE";
const size_t INVALID_IDX = std::numeric_limits<size_t>::max();
const std::string PHONEME_SEPARATOR = ",";

class PhonemeJudge
{
public:
   //! constructor
   PhonemeJudge(const std::string& c, const std::string& v) {
      {
         StringTokenizer st(c, PHONEME_SEPARATOR);
         size_t sz(st.size());
         for (size_t i(0); i < sz; ++i) {
            std::string phoneme(st.at(i));
            cutBlanks(phoneme);
            if (!phoneme.empty()) {
               this->consonants.insert(phoneme);
            }
         }
      }
      {
         StringTokenizer st(v, PHONEME_SEPARATOR);
         size_t sz(st.size());
         for (size_t i(0); i < sz; ++i) {
            std::string phoneme(st.at(i));
            cutBlanks(phoneme);
            if (!phoneme.empty()) {
               this->vowels.insert(phoneme);
            }
         }
      }
   }

   //! destructor
   virtual ~PhonemeJudge() {}

   //! return whether vowel or not
   const std::string& getType(const std::string& phoneme) const {
      if (consonants.end() != consonants.find(phoneme)) {
         return PhonemeInfo::TYPE_CONSONANT;
      }
      return PhonemeInfo::TYPE_VOWEL;
   }


private:
   //! copy constructor (donot use)
   PhonemeJudge(const PhonemeJudge&);

   //! assignment operator (donot use)
   PhonemeJudge& operator=(const PhonemeJudge&);

   //! consonants
   std::set<std::string> consonants;

   //! vowels
   std::set<std::string> vowels;
};

class InfoAdder
{
public:
   //! constructor
   InfoAdder(sinsy::IConvertable& c, const PhonemeJudge& pj) :
      convertable(c), phonemeJudge(pj), waiting(false), scoreFlag(0), macronFlag(false) {
   }

   //! destructor
   virtual ~InfoAdder() {
      reflect();
   }

   //! set score flag
   void setScoreFlag(ScoreFlag f) {
      scoreFlag = f;
   }

   //! set macron flag
   void setMacronFlag(bool f) {
      macronFlag = f;
   }

   //! add syllable
   void addSyllable(const PhonemeTable::PhonemeList& p) {
      if (p.empty()) { // fail safe
         WARN_MSG("Cannot add English syllable : no phonemes");
         return;
      }
      { // not cl
	  ptrList.push_back(new PhonemeTable::PhonemeList(p));
      }
   }

   //! get phonemes of last syllable
   const PhonemeTable::PhonemeList* getLastPhonemes() const {
      if (ptrList.empty()) {
         return NULL;
      }
      return ptrList.back();
   }

   //! get phonemes of last syllable
   PhonemeTable::PhonemeList* getLastPhonemes() {
      if (ptrList.empty()) {
         return NULL;
      }
      return ptrList.back();
   }

private:
   //! copy constructor (donot use)
   InfoAdder(const InfoAdder&);

   //! assignment operator (donot use)
   InfoAdder& operator=(const InfoAdder&);

   //! reflect to convertable
   void reflect() {
      if (ptrList.empty()) return;

      // add
      {
         std::string info = (macronFlag) ? "1" : "0";
         std::string lastPhoneme;
         std::vector<PhonemeTable::PhonemeList*>::iterator itr(ptrList.begin());
         const std::vector<PhonemeTable::PhonemeList*>::iterator itrEnd(ptrList.end());
         for (; itrEnd != itr; ++itr) {
            PhonemeTable::PhonemeList& phonemes(**itr);

            // same vowel
            while (!phonemes.empty()) {
               if (phonemes[0] != lastPhoneme) {
                  break;
               }
               phonemes.erase(phonemes.begin());
            }
            if (phonemes.empty()) {
               continue;
            }

            std::vector<PhonemeInfo> phonemeInfos;
            phonemeInfos.reserve(phonemes.size());
            const std::vector<std::string>::const_iterator iEnd(phonemes.end());
            for (std::vector<std::string>::const_iterator i(phonemes.begin()); iEnd != i; ++i) {
               const std::string& type(phonemeJudge.getType(*i));
               phonemeInfos.push_back(PhonemeInfo(type, *i, scoreFlag));
            }
            convertable.addInfo(phonemeInfos, LANGUAGE_INFO, info);
            if (PhonemeInfo::TYPE_VOWEL == phonemeJudge.getType(phonemes.back())) {
               lastPhoneme = phonemes.back();
            } else {
               lastPhoneme.clear();
            }
         }
      }

      // clear
      std::for_each(ptrList.begin(), ptrList.end(), Deleter<PhonemeTable::PhonemeList>());
      ptrList.clear();
   }

   //! target
   sinsy::IConvertable& convertable;

   //! phoneme type judge
   const PhonemeJudge& phonemeJudge;

   //! waiting flag
   bool waiting;

   //! score flag
   ScoreFlag scoreFlag;

   //! macron flag
   bool macronFlag;

   //! phoneme list
   std::vector<PhonemeTable::PhonemeList*> ptrList;
};

/*!
 convert string of char code to char
 */
bool str2char(const std::string& s, unsigned char& c)
{
   int tmp(-1);
   if ((2 < s.size()) && ('0' == s[0]) && ('x' == s[1])) {
      std::string sub(s.substr(2));
      std::istringstream iss(sub);
      iss >> std::hex >> tmp;
   } else {
      std::istringstream iss(s);
      iss >> tmp;
   }
   if ((tmp < 0) || (std::numeric_limits<unsigned char>::max() < tmp)) {
      ERR_MSG("Config of multibyte char range is wrong format: " << s);
      return false;
   }
   c = static_cast<unsigned char>(tmp);
   return true;
}

/*!
 set multibyte char range from str to mRange
 */
bool setMultibyteCharRange(MultibyteCharRange& mRange, const std::string& str)
{
   bool ret(true);
   StringTokenizer st1(str, ";");
   size_t size1(st1.size());
   for (size_t i(0); i < size1; ++i) {
      StringTokenizer st2(st1.at(i), ",");
      size_t size2(st2.size());
      if (3 != size2) {
         ERR_MSG("Config of multibyte char range is wrong format: " << str);
         return false;
      }

      size_t size(0);
      unsigned char begin(0), end(0);
      {
         std::istringstream iss(st2.at(0));
         iss >> size;
      }
      if (!str2char(st2.at(1), begin)) {
         ret = false;
         continue;
         // don't return false here to set other ranges
      }
      if (!str2char(st2.at(2), end)) {
         ret = false;
         continue;
         // don't return false here to set other ranges
      }

      if (false == mRange.addRange(size, begin, end)) {
         ret = false;
         continue;
         // don't return false here to set other ranges
      }
   }
   return ret;
}

/*!
 expand prevInfoAdder to infoAdder
 */
bool expand(InfoAdder& prevInfoAdder, InfoAdder& infoAdder, const MacronTable& macronTable)
{
   if (NULL != infoAdder.getLastPhonemes()) { // fail safe
      ERR_MSG("Dst InfoAdder is not empty (Source code is wrong)");
      return false;
   }

   PhonemeTable::PhonemeList* prevPhonemes(prevInfoAdder.getLastPhonemes());
   if (!prevPhonemes) {
      return false;
   }
   PhonemeTable::PhonemeList dst1;
   PhonemeTable::PhonemeList dst2;
   if (macronTable.divide(*prevPhonemes, dst1, dst2)) {
      *prevPhonemes = dst1;
      infoAdder.addSyllable(dst2);
   } else {
      // not "cl"
      dst2.push_back(prevPhonemes->back());
      infoAdder.addSyllable(dst2);
   }
   return true;
}

};

/*!
 constructor

 @param enc encoding strings (e.g. "utf_8, utf8, utf-8")
 */
EConf::EConf(const std::string& enc)
{
   StringTokenizer tokeizer(enc, SEPARATOR);
   size_t sz(tokeizer.size());

   for (size_t i(0); i < sz; ++i) {
      std::string e(tokeizer.at(i));
      cutBlanks(e);
      toLower(e);
      encodings.insert(e);
   }
}

/*!
 destructor
*/
EConf::~EConf()
{
}

/*!
 read phoneme table and config from files

 @param table phoneme table file path
 @param conf  config file path
 @return true if success
 */
bool EConf::read(const std::string& table, const std::string& conf, const std::string& macron)
{
   if (!phonemeTable.read(table)) {
      ERR_MSG("Cannot read phoneme table file : " << table);
      return false;
   }
   if (!config.read(conf)) {
      ERR_MSG("Cannot read config file : " << conf);
      phonemeTable.clear();
      return false;
   }
   if (!macronTable.read(macron)) {
      ERR_MSG("Cannot read macron table file : " << macron);
      macronTable.clear();
      return false;
   }

   // set multibyte char ranges
   std::string strCharRange(config.get(MULTIBYTE_CHAR_RANGE));
   if (!setMultibyteCharRange(multibyteCharRange, strCharRange)) {
      ERR_MSG("Failed to set multibyte char ranges");
      return false;
   }

   return true;
}

/*!
 convert lyrics to phonemes
*/
   bool EConf::convert(const std::string& enc, ConvertableList::iterator begin, ConvertableList::iterator end) const
   {
   // check encoding
   if (!checkEncoding(enc)) {
      return true; // no relation
   }

   const std::string macronSymbol(config.get(MACRON));
   const std::string delimiterSymbol(config.get(SYLLABLE_DELIMITER));
   std::string vowels(config.get(VOWELS));
   std::string consonants(config.get(CONSONANTS));

   PhonemeJudge phonemeJudge(consonants, vowels);

   std::vector<InfoAdder*> infoAdderList;
   std::queue<std::shared_ptr<PhonemeTable::PhonemeList>> queue;

   for (ConvertableList::iterator itr(begin); itr != end; ++itr) {
      InfoAdder* infoAdder = new InfoAdder(**itr, phonemeJudge);
      std::string lyric;
      Syllabic syllabic((*itr)->getSyllabic());
      if (Syllabic::BEGIN == syllabic)  {
	 // word ranges several notes
	 lyric = (*itr)->getLyric();
	  
	 ConvertableList::iterator intra_syl_itr(std::next(itr));
	 while (Syllabic::END != (*intra_syl_itr)->getSyllabic())  {
	    // Concatenate lyric until the end of syllables
	    lyric += (*intra_syl_itr)->getLyric();
	    ++intra_syl_itr;
	 }
	 // Add last syllable
	 lyric += (*intra_syl_itr)->getLyric();
      } else if (Syllabic::SINGLE == syllabic) {
	 lyric = (*itr)->getLyric();
      } else if (Syllabic::MIDDLE == syllabic || Syllabic::END == syllabic ) {
	 // Nothing to do
      }
      
      ScoreFlag scoreFlag(analyzeScoreFlags(lyric, &multibyteCharRange));
      
      infoAdder->setScoreFlag(scoreFlag);
      if (Syllabic::BEGIN == syllabic || Syllabic::SINGLE == syllabic)  {
	 while (!lyric.empty()) {
	    if (0 == lyric.compare(0, macronSymbol.size(), macronSymbol)) { // macron
	       if (infoAdderList.empty()) {
		  WARN_MSG("Macron have to follow another lyric");
	       } else {
		  expand(*(infoAdderList.back()), *infoAdder, macronTable);
	       }
	       infoAdder->setMacronFlag(true);
	       lyric.erase(0, macronSymbol.size());
	    } else { // others
	       PhonemeTable::Result result(phonemeTable.find(lyric));
	       if (!result.isValid()) {
		  break;
	       }
	       lyric.erase(0, result.getMatchedLength());
	       const PhonemeTable::PhonemeList* phonemes(result.getPhonemeList());

	       std::shared_ptr<PhonemeTable::PhonemeList> plptr (new PhonemeTable::PhonemeList);
	       queue.push(plptr);
	       for (int i(0); i < phonemes->size(); i++) {
		  const std::string phoneme = (*phonemes)[i];
		  if (0 == phoneme.compare(0, delimiterSymbol.size(), delimiterSymbol)) {
		     // New syllable
		     std::shared_ptr<PhonemeTable::PhonemeList> plptr (new PhonemeTable::PhonemeList);
		     queue.push(plptr);
		  } else {
		     // Add phoneme to the newest PhonemeTable::PhonemeList
		     queue.back()->push_back(phoneme);
		  }
	       }
	    }
         }
      }
      infoAdder->addSyllable((*queue.front().get()));
      // Delete the oldest PhonemeTable(Destructor should be called.)
      queue.pop();
      infoAdderList.push_back(infoAdder);
   }

   // clear list of InfoAdder
   std::for_each(infoAdderList.begin(), infoAdderList.end(), Deleter<InfoAdder>());
   infoAdderList.clear();

   return true;
}

/*!
 get sil string

 return sil str
 */
std::string EConf::getSilStr() const
{
   return SIL_STR;
}

/*!
 check encoding
 */
bool EConf::checkEncoding(const std::string& enc) const
{
   std::string encoding(enc);
   toLower(encoding);
   Encodings::const_iterator itr(encodings.find(encoding));
   return encodings.end() != itr;
}

/*!
 get multibyte char range
 */
const MultibyteCharRange& EConf::getMultibyteCharRange() const
{
   return multibyteCharRange;
}

};  // namespace sinsy
