/**
 * @file  TREgExprSearch.cxx
 * @brief TRE regex searching for Scintilla library
 *        (Scintilla Lib is copyright 1998-2017 by Neil Hodgson <neilh@scintilla.org>)
 *
 * uses TRE - Regular Expression Engine (v1.3)  - https://github.com/laurikari/tre/
 * 
 * @autor Rainer Kottenhoff (RaPeHoff)
 *
 */

#ifdef SCI_OWNREGEX

#include <stdlib.h>
#include <string>
#include <vector>

#pragma warning( push )
#pragma warning( disable : 4996 )   // Scintilla's "unsafe" use of std::copy() (SplitVector.h)
//                                  // or use -D_SCL_SECURE_NO_WARNINGS preprocessor define

#include "Platform.h"
#include "Scintilla.h"
#include "ILexer.h"
#include "SplitVector.h"
#include "Partitioning.h"
#include "CellBuffer.h"
#include "CaseFolder.h"
#include "RunStyles.h"
#include "Decoration.h"
#include "CharClassify.h"
#include "Document.h"
// ---------------------------------------------------------------
// TRE- Regular Expression Engine
#include "regex.h"
// ---------------------------------------------------------------

using namespace Scintilla;

#define SciPos(pos)    static_cast<Sci::Position>(pos)
#define SciLn(line)    static_cast<Sci::Line>(line)
#define SciPosExt(pos) static_cast<Sci_Position>(pos)

#define PosExt(n)      static_cast<long>(n)

// ---------------------------------------------------------------

class TREgExprSearch : public RegexSearchBase
{
public:

  explicit TREgExprSearch(CharClassify* charClassTable)
    : m_FindRegExPattern()
    , m_CompileFlags(-1)
    , m_CompiledRegExPattern()
    , m_CompileResult(REG_NOMATCH)
    , m_Groups()
    , m_RangeDocBegin(0)
    , m_RangeLength(0)
    , m_SubstitutionBuffer()
  {
  }

  virtual ~TREgExprSearch()
  {
    ClearPattern();
  }

  virtual long FindText(Document* doc, Sci::Position minPos, Sci::Position maxPos, const char* pattern,
                        bool caseSensitive, bool word, bool wordStart, int flags, Sci::Position* length) override;

  virtual const char* SubstituteByPosition(Document* doc, const char* text, Sci::Position* length) override;


private:

  __inline void ClearPattern()
  {
    m_FindRegExPattern.clear();
    m_CompileFlags = -1;
    tre_regfree(&m_CompiledRegExPattern);
    m_CompileResult = REG_NOMATCH;

    m_RangeDocBegin = 0;
    m_RangeLength = 0;

    for (auto& grp : m_Groups) {
      grp.rm_so = -1;
      grp.rm_eo = -1;
    }

    m_SubstitutionBuffer.clear();
  }


  std::string& translateRegExpr(std::string& regExprStr, bool wholeWord, bool wordStart, int eolMode);

  int getDocContextMatchFlags(Document* doc, int minPos, int maxPos);

  void adjustToEOLMode(Document* doc, int& matchResult);


private:

  std::string m_FindRegExPattern;
  int m_CompileFlags;
  regex_t m_CompiledRegExPattern;
  int m_CompileResult;

  static const size_t MAXGROUPS = 10;
  regmatch_t m_Groups[MAXGROUPS];

  Sci::Position  m_RangeDocBegin;
  Sci::Position  m_RangeLength;

  std::string m_SubstitutionBuffer;
};
// ============================================================================


RegexSearchBase *Scintilla::CreateRegexSearch(CharClassify *charClassTable)
{
  return new TREgExprSearch(charClassTable);
}

// ============================================================================

/**
 * forward declaration of utility functions
 */

// ============================================================================


/**
 * Find text in document, supporting both forward and backward
 * searches (just pass minPos > maxPos to do a backward search)
 * Has not been tested with backwards DBCS searches yet.
 */
long TREgExprSearch::FindText(Document* doc, Sci::Position minPos, Sci::Position maxPos, const char *pattern,
                              bool caseSensitive, bool word, bool wordStart, int flags, Sci::Position *length)
{
  const bool right2left = false; // always left-to-right match mode
  const bool extended = true;    // ignore spaces and use '#' as line-comment)

  // Range endpoints should not be inside DBCS characters, but just in case, move them.
  minPos = doc->MovePositionOutsideChar(minPos, 1, false);
  maxPos = doc->MovePositionOutsideChar(maxPos, 1, false);
  const bool findprevious = (minPos > maxPos);

  if (findprevious) {
    int tmp = minPos;  minPos = maxPos;  maxPos = tmp;
  }

  int compileFlags = REG_BASIC;
  compileFlags |= REG_NEWLINE; // the .(dot) does not match line-breaks
  compileFlags |= (extended) ? REG_EXTENDED : REG_BASIC;
  compileFlags |= (caseSensitive) ? REG_BASIC : REG_ICASE;
  compileFlags |= (right2left) ? REG_RIGHT_ASSOC : REG_BASIC;

  std::string transPattern = translateRegExpr(std::string(pattern), word, wordStart, doc->eolMode);

  bool bReCompile = ((m_CompileFlags != compileFlags) || (m_FindRegExPattern.compare(transPattern) != 0));

  if (bReCompile) 
  {
    ClearPattern();
    m_CompileFlags = compileFlags;
    m_FindRegExPattern = transPattern;
    try {
      m_CompileResult = tre_regcomp(&m_CompiledRegExPattern, m_FindRegExPattern.c_str(), compileFlags);
    }
    catch (...) {
      return (0 - REG_BADPAT);  // -1 is normally used for not found, -2 is used here for invalid regex
    }
  }

  switch (m_CompileResult) {
  case REG_OK:
    // all right
    break;
  case REG_NOMATCH:
    // n.a.
  case REG_BADPAT:
    // Invalid regexp.TRE returns this only if a multibyte character set is used in the current locale, and regex contained an invalid multibyte sequence.
  case REG_ECOLLATE:
    // Invalid collating element referenced.TRE returns this whenever equivalence classes or multicharacter collating elements are used in bracket expressions(they are not supported yet).
  case REG_ECTYPE:
    // Unknown character class name in[[:name:]].
  case REG_EESCAPE:
    // The last character of regex was a backslash(\).
  case REG_ESUBREG:
    // Invalid back reference; number in \digit invalid.
  case REG_EBRACK:
    // [] imbalance.
  case REG_EPAREN:
    // \(\) or () imbalance.
  case REG_EBRACE:
    // \{\} or {} imbalance.
  case REG_BADBR:
    // {} content invalid : not a number, more than two numbers, first larger than second, or number too large.
  case REG_ERANGE:
    // Invalid character range, e.g.ending point is earlier in the collating order than the starting point.
  case REG_ESPACE:
    // Out of memory, or an internal limit exceeded.
  case REG_BADRPT:
    // Invalid use of repetition operators : two or more repetition operators have been chained in an undefined way.
  default:
    return (0 - m_CompileResult);
  }

  // --- adjust line start and end positions for match ---


  int match = REG_NOMATCH;
  m_RangeDocBegin = minPos;
  m_RangeLength = (maxPos - minPos);

  if (findprevious)  // search previous 
  {
    // search for last occurrence in range
    Sci::Position rangeBegin = m_RangeDocBegin;
    Sci::Position rangLength = m_RangeLength;
    do {
      match = tre_regnexec(&m_CompiledRegExPattern, // don't care for sub groups in while loop
        doc->RangePointer(rangeBegin, rangLength), (size_t)rangLength, 1, m_Groups,
        getDocContextMatchFlags(doc, rangeBegin, rangeBegin + rangLength));

      if (match == REG_OK) {
        // save Range
        m_RangeDocBegin = rangeBegin;
        m_RangeLength = rangLength;
        // prepare next match
        rangeBegin += (m_Groups[0].rm_so + 1);
        rangLength = maxPos - rangeBegin;
      }
    } 
    while ((match == REG_OK) && (rangeBegin < maxPos));
  }

  // --- find match in range and set region groups ---
  match = tre_regnexec(&m_CompiledRegExPattern,
    doc->RangePointer(m_RangeDocBegin, m_RangeLength), (size_t)m_RangeLength, MAXGROUPS, m_Groups, 
    getDocContextMatchFlags(doc, m_RangeDocBegin, m_RangeDocBegin + m_RangeLength));
 

  //if ((match == REG_OK) && (m_CompiledRegExPattern.re_nsub >= MAXGROUPS)) {
  //  //TODO: show warning
  //}

  adjustToEOLMode(doc, match);

  //NOTE: potential 64-bit-size issue at interface here:
  *length = ((match == REG_OK) ? SciPos(m_Groups[0].rm_eo - m_Groups[0].rm_so) : -1);
  return  ((match == REG_OK) ? PosExt(m_RangeDocBegin + SciPos(m_Groups[0].rm_so)) : PosExt(0 - REG_NOMATCH));
}
// ============================================================================


const char* TREgExprSearch::SubstituteByPosition(Document* doc, const char* text, Sci::Position* length)
{
  if (m_Groups[0].rm_so < 0) {
    *length = SciPos(-1);
    return nullptr;
  }

  m_SubstitutionBuffer.clear();

  for (int j = 0; j < *length; j++) {
    if (text[j] == '\\') 
    {
      if ((text[j+1] >= '0') && (text[j+1] <= '9')) 
      {
        unsigned int patNum = text[j+1] - '0';
        
        Sci::Position len = (m_Groups[patNum].rm_eo - m_Groups[patNum].rm_so);

        if (len > 0) // Will be 0 if try for a match that did not occur
          m_SubstitutionBuffer.append(doc->RangePointer(SciPos(m_RangeDocBegin + m_Groups[patNum].rm_so),SciPos(len)), len);
        
        ++j;
      }
      else {
        ++j;
        switch (text[j]) {
        case 'a':
          m_SubstitutionBuffer.push_back('\a');
          break;
        case 'b':
          m_SubstitutionBuffer.push_back('\b');
          break;
        case 'f':
          m_SubstitutionBuffer.push_back('\f');
          break;
        case 'n':
          m_SubstitutionBuffer.push_back('\n');
          break;
        case 'r':
          m_SubstitutionBuffer.push_back('\r');
          break;
        case 't':
          m_SubstitutionBuffer.push_back('\t');
          break;
        case 'v':
          m_SubstitutionBuffer.push_back('\v');
          break;
        case '\\':
          m_SubstitutionBuffer.push_back('\\');
          break;
        default:
          m_SubstitutionBuffer.push_back('\\');
          --j;
          break;
        }
      }
    }
    else {
      m_SubstitutionBuffer.push_back(text[j]);
    }
  }

  //NOTE: potential 64-bit-size issue at interface here:
  *length = SciPos(m_SubstitutionBuffer.length());
  return m_SubstitutionBuffer.c_str();
}
// ============================================================================





// ----------------------------------------------------------------------------------------------
// --- correct EOL ($) matches:  TRE's  $  matches only empty string immediately before '\n'  ---
// ----------------------------------------------------------------------------------------------

void TREgExprSearch::adjustToEOLMode(Document* doc, int& matchResult)
{
  if (matchResult == REG_OK) {

    switch (doc->eolMode) {
    case SC_EOL_LF:
      // we are fine here
      break;

    case SC_EOL_CR:
      //TODO: don't know what to do here ...
      break;

    case SC_EOL_CRLF:
      {
        char eos = (!m_FindRegExPattern.empty()) ? m_FindRegExPattern.back() : '\0';
        char esc = (m_FindRegExPattern.length() >= 2) ? m_FindRegExPattern.at(m_FindRegExPattern.length() - 2) : '\0';

        if (('$' == eos) && ('\\' != esc)) {
          Sci::Position matchRelEnd = m_Groups[0].rm_eo;
          Sci::Position matchEndDocPos = m_RangeDocBegin + matchRelEnd - 1;
          if ((matchEndDocPos >= 0) && ('\r' == doc->CharAt(matchEndDocPos))) {
            for (auto& grp : m_Groups) {
              if (grp.rm_so == matchRelEnd) { --(grp.rm_so); }
              if (grp.rm_eo == matchRelEnd) { --(grp.rm_eo); }
            }
          }
          else { // not a CRLF EOL - wrong match
            matchResult = REG_NOMATCH;
            m_Groups[0].rm_so = -1;
            m_Groups[0].rm_eo = -1;
          }
        }
      }
      break;
    }
  }
}
// ============================================================================



int TREgExprSearch::getDocContextMatchFlags(Document* doc, Sci::Position minPos, Sci::Position maxPos)
{
  //Sci::Line linesTotal = doc->LinesTotal();
  Sci::Position fileLastPos = SciPos(doc->Length());

  Sci::Position lineOfMinPos = SciPos(doc->LineFromPosition(SciPosExt(minPos)));
  Sci::Position lineOfMaxPos = SciPos(doc->LineFromPosition(SciPosExt(maxPos)));

  Sci::Position lineStartOfMinPos = SciPos(doc->LineStart(lineOfMinPos));
  Sci::Position lineEndOfMaxPos   = SciPos(doc->LineEnd(lineOfMaxPos));

  int matchFlags = 0;
  matchFlags |= (lineStartOfMinPos == minPos) ? 0 : REG_NOTBOL;
  matchFlags |= (lineEndOfMaxPos == maxPos) ? 0 : REG_NOTEOL;

  return matchFlags;
}
// ============================================================================



void replaceAll(std::string& source, const std::string& from, const std::string& to)
{
  std::string newString;
  newString.reserve(source.length() * 2);  // avoids a few memory allocations

  std::string::size_type lastPos = 0;
  std::string::size_type findPos;

  while (std::string::npos != (findPos = source.find(from,lastPos))) {
    newString.append(source,lastPos,findPos - lastPos);
    newString += to;
    lastPos = findPos + from.length();
  }
  // Care for the rest after last occurrence
  newString += source.substr(lastPos);

  source.swap(newString);
}
// ============================================================================



std::string& TREgExprSearch::translateRegExpr(std::string& regExprStr, bool wholeWord, bool wordStart, int eolMode)
{
  std::string	tmpStr;

  if (wholeWord || wordStart) {      // push '\b' at the begin of regexpr
    tmpStr.push_back('\\');
    tmpStr.push_back('b');
    tmpStr.append(regExprStr);
    if (wholeWord) {               // push '\b' at the end of regexpr
      tmpStr.push_back('\\');
      tmpStr.push_back('b');
    }
    replaceAll(tmpStr, ".", R"(\w)");
  }
  else {
    tmpStr.append(regExprStr);
  }

  switch (eolMode) {
  case SC_EOL_LF:
    // we are fine here
    break;

  case SC_EOL_CR:
    //TODO: don't know what to do here ...
    break;

  case SC_EOL_CRLF:
    {
      replaceAll(tmpStr, "$", R"(\r$)");
      replaceAll(tmpStr, R"(\\r$)", R"(\$)");
    }
    break;
  }

  std::swap(regExprStr, tmpStr);
  return regExprStr;
}
// ============================================================================


// ============================================================================

#pragma warning( pop )

#endif //SCI_OWNREGEX
