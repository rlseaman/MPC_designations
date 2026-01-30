/*
 * MPCDesignation.cs - Convert between packed and unpacked MPC designations
 *
 * Based on Minor Planet Center specifications:
 * https://www.minorplanetcenter.net/iau/info/PackedDes.html
 */

using System;
using System.Text;
using System.Text.RegularExpressions;

namespace MPC
{
    public class MPCDesignationException : Exception
    {
        public MPCDesignationException(string message) : base(message) { }
    }

    public enum Format
    {
        Unknown,
        Packed,
        Unpacked
    }

    public enum DesignationType
    {
        Unknown,
        Permanent,
        Provisional,
        ProvisionalExtended,
        Survey,
        CometNumbered,
        CometProvisional,
        CometFull,
        CometAncient,
        CometBCE,
        Satellite
    }

    public class FormatInfo
    {
        public Format Format { get; set; } = Format.Unknown;
        public DesignationType Type { get; set; } = DesignationType.Unknown;
        public string Subtype { get; set; } = "";
    }

    public class ConversionResult
    {
        public string Input { get; set; } = "";
        public string Output { get; set; } = "";
        public FormatInfo Info { get; set; } = new FormatInfo();
    }

    public static class MPCDesignation
    {
        public const string VERSION = "1.0.0";
        public const long MAX_ASTEROID_NUMBER = 15396335L;

        private const string BASE62_CHARS = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
        private const string CENTURY_LETTERS = "ABCDEFGHIJKL";
        private const string COMET_TYPES = "PCDXAI";
        private const string SATELLITE_PLANETS = "JSUN";

        // =========================================================================
        // Utility functions
        // =========================================================================

        private static string Trim(string s) => s.Trim(' ');

        private static void ValidateRawInput(string s)
        {
            foreach (char c in s)
            {
                if (c < 32 || c > 126)
                {
                    throw new MPCDesignationException($"Invalid character in designation: '\\x{(int)c:x2}'");
                }
            }
        }

        private static void ValidateWhitespace(string s)
        {
            bool prevSpace = false;
            foreach (char c in s)
            {
                if (c < 32 || c > 126)
                {
                    throw new MPCDesignationException("Invalid character in designation");
                }
                if (c == ' ')
                {
                    if (prevSpace)
                    {
                        throw new MPCDesignationException("Consecutive spaces in designation");
                    }
                    prevSpace = true;
                }
                else
                {
                    prevSpace = false;
                }
            }
        }

        private static int Base62ToNum(char c)
        {
            int idx = BASE62_CHARS.IndexOf(c);
            if (idx < 0)
            {
                throw new MPCDesignationException($"Invalid base-62 character: {c}");
            }
            return idx;
        }

        private static char NumToBase62(int n)
        {
            if (n < 0 || n > 61)
            {
                throw new MPCDesignationException($"Number out of base-62 range: {n}");
            }
            return BASE62_CHARS[n];
        }

        private static long Base62StringToNum(string s)
        {
            long result = 0;
            foreach (char c in s)
            {
                result = result * 62 + Base62ToNum(c);
            }
            return result;
        }

        private static string NumToBase62String(long n, int width)
        {
            char[] result = new char[width];
            for (int i = width - 1; i >= 0; i--)
            {
                result[i] = NumToBase62((int)(n % 62));
                n /= 62;
            }
            return new string(result);
        }

        private static int CenturyToCode(int century)
        {
            if (century < 10 || century > 21) return -1;
            return CENTURY_LETTERS[century - 10];
        }

        private static int CodeToCentury(char code)
        {
            if (code >= 'A' && code <= 'L') return code - 'A' + 10;
            return -1;
        }

        private static int LetterToPosition(char letter)
        {
            if (letter < 'A' || letter > 'Z')
            {
                throw new MPCDesignationException($"Invalid half-month letter: {letter}");
            }
            int pos = letter - 'A' + 1;
            if (letter > 'I') pos--;
            return pos;
        }

        private static char PositionToLetter(int pos)
        {
            if (pos < 1 || pos > 25)
            {
                throw new MPCDesignationException($"Invalid letter position: {pos}");
            }
            int p = pos;
            if (p >= 9) p++;
            return (char)('A' + p - 1);
        }

        private static int DecodeCycleCount(string encoded)
        {
            if (encoded.Length < 2)
            {
                throw new MPCDesignationException("Invalid cycle count encoding");
            }

            char first = encoded[0];
            char second = encoded[1];
            int tens;

            if (first >= '0' && first <= '9')
            {
                tens = first - '0';
            }
            else if (first >= 'A' && first <= 'Z')
            {
                tens = first - 'A' + 10;
            }
            else if (first >= 'a' && first <= 'z')
            {
                tens = first - 'a' + 36;
            }
            else
            {
                throw new MPCDesignationException("Invalid cycle count encoding");
            }

            if (second < '0' || second > '9')
            {
                throw new MPCDesignationException("Invalid cycle count encoding");
            }

            return tens * 10 + (second - '0');
        }

        private static string EncodeCycleCount(int count)
        {
            if (count < 0 || count >= 620)
            {
                throw new MPCDesignationException($"Cycle count out of range (0-619): {count}");
            }

            int tens = count / 10;
            int ones = count % 10;
            char first;

            if (tens < 10)
            {
                first = (char)('0' + tens);
            }
            else if (tens < 36)
            {
                first = (char)('A' + tens - 10);
            }
            else
            {
                first = (char)('a' + tens - 36);
            }

            return $"{first}{ones}";
        }

        private static bool IsCometType(char c) => COMET_TYPES.Contains(c);
        private static bool IsSatellitePlanet(char c) => SATELLITE_PLANETS.Contains(c);
        private static bool IsValidHalfMonth(char c) => c >= 'A' && c <= 'Y' && c != 'I';

        private static string GetCometTypeName(char type)
        {
            return type switch
            {
                'P' => "periodic",
                'C' => "non-periodic",
                'D' => "defunct",
                'X' => "uncertain orbit",
                'A' => "asteroid with comet designation",
                'I' => "interstellar",
                _ => "unknown"
            };
        }

        private static string GetPlanetName(char code)
        {
            return code switch
            {
                'J' => "Jupiter",
                'S' => "Saturn",
                'U' => "Uranus",
                'N' => "Neptune",
                _ => "unknown"
            };
        }

        // =========================================================================
        // Permanent (numbered) asteroid designations
        // =========================================================================

        public static string PackPermanent(long number)
        {
            if (number < 1 || number > MAX_ASTEROID_NUMBER)
            {
                throw new MPCDesignationException($"Invalid asteroid number: {number}");
            }

            if (number < 100000)
            {
                return number.ToString("D5");
            }

            if (number < 620000)
            {
                int div = (int)(number / 10000);
                int mod = (int)(number % 10000);
                char letter = div < 36 ? (char)('A' + div - 10) : (char)('a' + div - 36);
                return $"{letter}{mod:D4}";
            }

            long offset = number - 620000;
            return "~" + NumToBase62String(offset, 4);
        }

        public static long UnpackPermanent(string packed)
        {
            string p = Trim(packed);
            if (p.Length != 5)
            {
                throw new MPCDesignationException("Invalid packed permanent designation length");
            }

            char first = p[0];

            if (first == '~')
            {
                return 620000 + Base62StringToNum(p.Substring(1, 4));
            }

            if (first >= '0' && first <= '9')
            {
                return long.Parse(p);
            }

            if (first >= 'A' && first <= 'Z')
            {
                int val = first - 'A' + 10;
                return val * 10000L + long.Parse(p.Substring(1, 4));
            }

            if (first >= 'a' && first <= 'z')
            {
                int val = first - 'a' + 36;
                return val * 10000L + long.Parse(p.Substring(1, 4));
            }

            throw new MPCDesignationException("Invalid packed permanent designation");
        }

        // =========================================================================
        // Provisional asteroid designations
        // =========================================================================

        public static string PackProvisional(string unpacked)
        {
            string u = Trim(unpacked);

            // Check for survey designations
            var surveyMatch = Regex.Match(u, @"^(\d+) (P-L|T-[123])$");
            if (surveyMatch.Success)
            {
                int number = int.Parse(surveyMatch.Groups[1].Value);
                string survey = surveyMatch.Groups[2].Value;
                if (number < 1)
                {
                    throw new MPCDesignationException("Survey number must be positive");
                }
                string prefix = survey switch
                {
                    "P-L" => "PLS",
                    "T-1" => "T1S",
                    "T-2" => "T2S",
                    "T-3" => "T3S",
                    _ => throw new MPCDesignationException("Invalid survey type")
                };
                return $"{prefix}{number:D4}";
            }

            // Check for old-style designation: "A908 CJ" or "B842 FA"
            var oldStyleMatch = Regex.Match(u, @"^([AB])(\d)(\d{2}) ([A-Z])([A-Z])$");
            if (oldStyleMatch.Success)
            {
                int centuryDigit = int.Parse(oldStyleMatch.Groups[2].Value);
                int yearShort = int.Parse(oldStyleMatch.Groups[3].Value);
                char halfMonth = oldStyleMatch.Groups[4].Value[0];
                char secondLetter = oldStyleMatch.Groups[5].Value[0];

                char centuryCode = centuryDigit switch
                {
                    8 => 'I',
                    9 => 'J',
                    0 => 'K',
                    _ => throw new MPCDesignationException("Invalid century digit in old-style designation")
                };

                return $"{centuryCode}{yearShort:D2}{halfMonth}00{secondLetter}";
            }

            // Standard provisional: "1995 XA" or "1995 XA12"
            var provMatch = Regex.Match(u, @"^(\d{4}) ([A-Z])([A-Z])(\d*)$");
            if (!provMatch.Success)
            {
                throw new MPCDesignationException($"Invalid unpacked provisional designation: {u}");
            }

            int year = int.Parse(provMatch.Groups[1].Value);
            char hm = provMatch.Groups[2].Value[0];
            char sl = provMatch.Groups[3].Value[0];
            string orderStr = provMatch.Groups[4].Value;

            if (!IsValidHalfMonth(hm))
            {
                throw new MPCDesignationException($"Invalid half-month letter: {hm}");
            }

            int century = year / 100;
            int ys = year % 100;

            int centuryCodeInt = CenturyToCode(century);
            if (centuryCodeInt < 0)
            {
                throw new MPCDesignationException($"Invalid century in year: {year}");
            }
            char cc = (char)centuryCodeInt;

            long orderNum = 0;
            if (!string.IsNullOrEmpty(orderStr))
            {
                if (!long.TryParse(orderStr, out orderNum) || orderNum < 0)
                {
                    throw new MPCDesignationException($"Cycle count out of range (overflow): {orderStr}");
                }
            }

            // Check if we need extended format
            if (orderNum >= 620)
            {
                int yearDigit = year % 100;
                long baseSequence = (orderNum - 620) * 25 + LetterToPosition(sl) - 1;
                string seqEncoded = NumToBase62String(baseSequence, 4);
                return $"_{NumToBase62(yearDigit)}{hm}{seqEncoded}";
            }

            string orderEncoded = EncodeCycleCount((int)orderNum);
            return $"{cc}{ys:D2}{hm}{orderEncoded}{sl}";
        }

        public static string UnpackProvisional(string packed)
        {
            string p = Trim(packed);

            // Check for survey designations
            if (p.Length == 7)
            {
                if (p.StartsWith("PLS"))
                {
                    int num = int.Parse(p.Substring(3, 4));
                    return $"{num} P-L";
                }
                if (p.StartsWith("T1S"))
                {
                    int num = int.Parse(p.Substring(3, 4));
                    return $"{num} T-1";
                }
                if (p.StartsWith("T2S"))
                {
                    int num = int.Parse(p.Substring(3, 4));
                    return $"{num} T-2";
                }
                if (p.StartsWith("T3S"))
                {
                    int num = int.Parse(p.Substring(3, 4));
                    return $"{num} T-3";
                }
            }

            // Extended format with underscore
            if (p.Length == 7 && p[0] == '_')
            {
                int yearDigit = Base62ToNum(p[1]);
                char halfMonth = p[2];
                long baseSequence = Base62StringToNum(p.Substring(3, 4));

                int cycle = 620 + (int)(baseSequence / 25);
                int letterPos = (int)(baseSequence % 25) + 1;
                char secondLetter = PositionToLetter(letterPos);

                int year = 2000 + yearDigit;

                return $"{year} {halfMonth}{secondLetter}{cycle}";
            }

            if (p.Length != 7)
            {
                throw new MPCDesignationException("Invalid packed provisional designation length");
            }

            char centuryCode = p[0];
            int century = CodeToCentury(centuryCode);
            if (century < 0)
            {
                throw new MPCDesignationException($"Invalid century code: {centuryCode}");
            }

            int yearShort = int.Parse(p.Substring(1, 2));
            char hm = p[3];
            int orderNum = DecodeCycleCount(p.Substring(4, 2));
            char sl = p[6];

            int fullYear = century * 100 + yearShort;

            if (orderNum == 0)
            {
                return $"{fullYear} {hm}{sl}";
            }
            return $"{fullYear} {hm}{sl}{orderNum}";
        }

        // =========================================================================
        // Comet provisional designations
        // =========================================================================

        private static string PackCometProvisional(string unpacked)
        {
            string u = Trim(unpacked);

            var match = Regex.Match(u, @"^(\d{4}) ([A-Z])(\d+)(?:-([A-Z]{1,2}))?$");
            if (!match.Success)
            {
                throw new MPCDesignationException($"Invalid unpacked comet provisional designation: {u}");
            }

            int year = int.Parse(match.Groups[1].Value);
            char halfMonth = match.Groups[2].Value[0];
            int orderNum = int.Parse(match.Groups[3].Value);
            string fragment = match.Groups[4].Success ? match.Groups[4].Value : "";

            if (orderNum < 1)
            {
                throw new MPCDesignationException("Comet order number must be positive");
            }

            int century = year / 100;
            int yearShort = year % 100;

            int centuryCodeInt = CenturyToCode(century);
            if (centuryCodeInt < 0)
            {
                throw new MPCDesignationException($"Invalid century in year: {year}");
            }
            char centuryCode = (char)centuryCodeInt;

            string orderEncoded = EncodeCycleCount(orderNum);
            string fragmentCode = string.IsNullOrEmpty(fragment) ? "0" : fragment.ToLower();

            return $"{centuryCode}{yearShort:D2}{halfMonth}{orderEncoded}{fragmentCode}";
        }

        private static string UnpackCometProvisional(string packed)
        {
            string p = Trim(packed);
            int len = p.Length;

            if (len != 7 && len != 8)
            {
                throw new MPCDesignationException("Invalid packed comet provisional designation length");
            }

            char centuryCode = p[0];
            int century = CodeToCentury(centuryCode);
            if (century < 0)
            {
                throw new MPCDesignationException($"Invalid century code: {centuryCode}");
            }

            int yearShort = int.Parse(p.Substring(1, 2));
            char halfMonth = p[3];
            int orderNum = DecodeCycleCount(p.Substring(4, 2));
            string fragment = len == 7 ? p.Substring(6, 1) : p.Substring(6, 2);

            int fullYear = century * 100 + yearShort;

            var sb = new StringBuilder();
            sb.Append($"{fullYear} {halfMonth}{orderNum}");
            if (fragment != "0" && fragment[0] != '0')
            {
                sb.Append($"-{fragment.ToUpper()}");
            }
            return sb.ToString();
        }

        // =========================================================================
        // Numbered comet designations
        // =========================================================================

        private static string PackCometNumbered(string unpacked)
        {
            string u = Trim(unpacked);

            var match = Regex.Match(u, @"^(\d+)([PD])(?:/[A-Za-z].*)?$");
            if (!match.Success)
            {
                throw new MPCDesignationException("Invalid unpacked numbered comet designation");
            }

            int number = int.Parse(match.Groups[1].Value);
            char cometType = match.Groups[2].Value[0];

            if (number < 1 || number > 9999)
            {
                throw new MPCDesignationException($"Comet number out of range (1-9999): {number}");
            }

            return $"{number:D4}{cometType}";
        }

        private static string UnpackCometNumbered(string packed)
        {
            string p = Trim(packed);

            var match = Regex.Match(p, @"^(\d{4})([PD])$");
            if (!match.Success)
            {
                throw new MPCDesignationException("Invalid packed numbered comet designation");
            }

            int number = int.Parse(match.Groups[1].Value);
            char cometType = match.Groups[2].Value[0];

            return $"{number}{cometType}";
        }

        // =========================================================================
        // Satellite designations
        // =========================================================================

        private static string PackSatellite(string unpacked)
        {
            string u = Trim(unpacked);

            var match = Regex.Match(u, @"^S/(\d{4}) ([JSUN]) (\d+)$");
            if (!match.Success)
            {
                throw new MPCDesignationException("Invalid unpacked satellite designation");
            }

            int year = int.Parse(match.Groups[1].Value);
            char planet = match.Groups[2].Value[0];
            int number = int.Parse(match.Groups[3].Value);

            if (number < 1)
            {
                throw new MPCDesignationException("Satellite number must be positive");
            }

            int century = year / 100;
            int yearShort = year % 100;

            int centuryCodeInt = CenturyToCode(century);
            if (centuryCodeInt < 0)
            {
                throw new MPCDesignationException($"Invalid century in year: {year}");
            }
            char centuryCode = (char)centuryCodeInt;

            string numberEncoded = EncodeCycleCount(number);

            return $"S{centuryCode}{yearShort:D2}{planet}{numberEncoded}0";
        }

        private static string UnpackSatellite(string packed)
        {
            string p = Trim(packed);

            if (p.Length != 8 || p[0] != 'S')
            {
                throw new MPCDesignationException("Invalid packed satellite designation");
            }

            char centuryCode = p[1];
            int century = CodeToCentury(centuryCode);
            if (century < 0)
            {
                throw new MPCDesignationException($"Invalid century code: {centuryCode}");
            }

            int yearShort = int.Parse(p.Substring(2, 2));
            char planet = p[4];

            if (!IsSatellitePlanet(planet))
            {
                throw new MPCDesignationException($"Invalid planet code: {planet}");
            }

            int number = DecodeCycleCount(p.Substring(5, 2));
            int fullYear = century * 100 + yearShort;

            return $"S/{fullYear} {planet} {number}";
        }

        // =========================================================================
        // BCE year encoding
        // =========================================================================

        private static (char prefix, string code) EncodeBCEYear(int year)
        {
            if (year >= 0)
            {
                throw new MPCDesignationException($"Not a BCE year: {year}");
            }

            int absYear = -year;
            int code = 99 - (absYear % 100);

            char prefix;
            if (absYear < 100)
            {
                prefix = '/';
            }
            else if (absYear < 200)
            {
                prefix = '.';
            }
            else if (absYear < 300)
            {
                prefix = '-';
            }
            else
            {
                throw new MPCDesignationException($"BCE year out of supported range: {year}");
            }

            return (prefix, code.ToString("D2"));
        }

        private static int DecodeBCEYear(char prefix, string code)
        {
            int codeNum = int.Parse(code);
            int yearPart = 99 - codeNum;

            return prefix switch
            {
                '/' => -yearPart,
                '.' => -(yearPart + 100),
                '-' => -(yearPart + 200),
                _ => throw new MPCDesignationException($"Invalid BCE prefix: {prefix}")
            };
        }

        // =========================================================================
        // Ancient/BCE comet designations
        // =========================================================================

        private static string PackAncientComet(string unpacked)
        {
            string u = Trim(unpacked);

            var match = Regex.Match(u, @"^([PCDXAI])/(-?\d+) ([A-Z])(\d+)(?:-([A-Z]))?$");
            if (!match.Success)
            {
                throw new MPCDesignationException("Invalid ancient comet designation");
            }

            char cometType = match.Groups[1].Value[0];
            int year = int.Parse(match.Groups[2].Value);
            char halfMonth = match.Groups[3].Value[0];
            int orderNum = int.Parse(match.Groups[4].Value);
            string fragment = match.Groups[5].Success ? match.Groups[5].Value : "";

            if (orderNum < 1)
            {
                throw new MPCDesignationException("Comet order number must be positive");
            }

            string orderEncoded = EncodeCycleCount(orderNum);
            char fragmentCode = string.IsNullOrEmpty(fragment) ? '0' : char.ToLower(fragment[0]);

            if (year < 0)
            {
                var (prefix, yearCode) = EncodeBCEYear(year);
                return $"{cometType}{prefix}{yearCode}{halfMonth}{orderEncoded}{fragmentCode}";
            }

            return $"{cometType}{year:D3}{halfMonth}{orderEncoded}{fragmentCode}";
        }

        private static string UnpackAncientComet(string packed)
        {
            string p = Trim(packed);

            if (p.Length != 8)
            {
                throw new MPCDesignationException("Invalid ancient comet designation length");
            }

            char cometType = p[0];
            if (!IsCometType(cometType))
            {
                throw new MPCDesignationException($"Invalid comet type: {cometType}");
            }

            int year;
            char halfMonth;
            int orderNum;
            char fragment;

            if (p[1] == '/' || p[1] == '.' || p[1] == '-')
            {
                year = DecodeBCEYear(p[1], p.Substring(2, 2));
                halfMonth = p[4];
                orderNum = DecodeCycleCount(p.Substring(5, 2));
                fragment = p[7];
            }
            else
            {
                year = int.Parse(p.Substring(1, 3));
                halfMonth = p[4];
                orderNum = DecodeCycleCount(p.Substring(5, 2));
                fragment = p[7];
            }

            var sb = new StringBuilder();
            sb.Append($"{cometType}/{year} {halfMonth}{orderNum}");
            if (fragment != '0')
            {
                sb.Append($"-{char.ToUpper(fragment)}");
            }
            return sb.ToString();
        }

        // =========================================================================
        // Full comet designations
        // =========================================================================

        private static bool IsAsteroidStylePacked(string provisional)
        {
            if (provisional.Length != 7) return false;
            return char.IsUpper(provisional[6]);
        }

        private static bool IsAsteroidStyleUnpacked(string provisional)
        {
            int spacePos = provisional.IndexOf(' ');
            if (spacePos < 0 || spacePos + 2 >= provisional.Length) return false;
            return char.IsLetter(provisional[spacePos + 2]);
        }

        private static string PackCometFull(string unpacked)
        {
            string u = Trim(unpacked);

            var match = Regex.Match(u, @"^(\d*)([PCDXAI])/(-?\d+) (.+)$");
            if (!match.Success)
            {
                throw new MPCDesignationException($"Invalid unpacked comet designation: {u}");
            }

            string numberStr = match.Groups[1].Value;
            char cometType = match.Groups[2].Value[0];
            int year = int.Parse(match.Groups[3].Value);
            string provPart = match.Groups[4].Value;

            // Check for ancient or BCE year
            if (year < 1000)
            {
                return PackAncientComet(u);
            }

            // Modern comet
            string provisional = $"{year} {provPart}";
            string provisionalPacked = IsAsteroidStyleUnpacked(provisional)
                ? PackProvisional(provisional)
                : PackCometProvisional(provisional);

            if (string.IsNullOrEmpty(numberStr))
            {
                return $"{cometType}{provisionalPacked}";
            }

            int num = int.Parse(numberStr);
            if (num < 1 || num > 9999)
            {
                throw new MPCDesignationException($"Comet number out of range (1-9999): {num}");
            }

            return $"{num:D4}{cometType}{provisionalPacked}";
        }

        private static string UnpackCometFull(string packed)
        {
            string p = packed; // Don't trim - need leading spaces for 12-char format
            int len = p.Length;

            char cometType;
            string provisionalPart;
            int number = 0;

            if (len == 8)
            {
                cometType = p[0];
                provisionalPart = p.Substring(1, 7);
            }
            else if (len == 9)
            {
                cometType = p[0];
                provisionalPart = p.Substring(1, 8);
            }
            else if (len == 12 || (len < 12 && p[0] == ' '))
            {
                while (p.Length < 12)
                {
                    p = " " + p;
                }
                string numPart = p.Substring(0, 4);
                cometType = p[4];
                provisionalPart = p.Substring(5, 7);

                string trimmedNum = numPart.Trim();
                if (!string.IsNullOrEmpty(trimmedNum))
                {
                    number = int.Parse(trimmedNum);
                }
            }
            else
            {
                throw new MPCDesignationException("Invalid packed full comet designation length");
            }

            if (!IsCometType(cometType))
            {
                throw new MPCDesignationException($"Invalid comet type: {cometType}");
            }

            string provisional = IsAsteroidStylePacked(provisionalPart)
                ? UnpackProvisional(provisionalPart)
                : UnpackCometProvisional(provisionalPart);

            if (number == 0)
            {
                return $"{cometType}/{provisional}";
            }
            return $"{number}{cometType}/{provisional}";
        }

        // =========================================================================
        // Format detection
        // =========================================================================

        public static FormatInfo DetectFormat(string designation)
        {
            var info = new FormatInfo();

            ValidateRawInput(designation);

            int origLen = designation.Length;

            // Check for packed 12-char comet before trimming
            if (origLen == 12 && IsCometType(designation[4]))
            {
                info.Format = Format.Packed;
                info.Type = DesignationType.CometFull;
                info.Subtype = "comet with provisional designation (12-char)";
                return info;
            }

            // Check for packed 8-char comet
            if (origLen == 8 && IsCometType(designation[0]) &&
                designation[1] >= 'A' && designation[1] <= 'L')
            {
                info.Format = Format.Packed;
                info.Type = DesignationType.CometFull;
                info.Subtype = "comet with provisional designation (8-char)";
                return info;
            }

            // Check for packed 9-char comet with 2-letter fragment
            if (origLen == 9 && IsCometType(designation[0]) &&
                designation[1] >= 'A' && designation[1] <= 'L' &&
                char.IsLower(designation[7]) && char.IsLower(designation[8]))
            {
                info.Format = Format.Packed;
                info.Type = DesignationType.CometFull;
                info.Subtype = "comet with provisional designation (9-char, 2-letter fragment)";
                return info;
            }

            // Check for packed ancient comet
            if (origLen == 8 && IsCometType(designation[0]) &&
                char.IsDigit(designation[1]) && char.IsDigit(designation[2]) && char.IsDigit(designation[3]))
            {
                info.Format = Format.Packed;
                info.Type = DesignationType.CometAncient;
                info.Subtype = "comet with ancient provisional (year < 1000)";
                return info;
            }

            // Check for packed BCE comet
            if (origLen == 8 && IsCometType(designation[0]) &&
                (designation[1] == '/' || designation[1] == '.' || designation[1] == '-') &&
                !designation.Contains(' '))
            {
                info.Format = Format.Packed;
                info.Type = DesignationType.CometBCE;
                info.Subtype = "comet with BCE provisional";
                return info;
            }

            string des = Trim(designation);
            int len = des.Length;

            ValidateWhitespace(des);

            // Check for packed satellite
            if (len == 8 && des[0] == 'S' && des[1] >= 'A' && des[1] <= 'L')
            {
                info.Format = Format.Packed;
                info.Type = DesignationType.Satellite;
                info.Subtype = $"natural satellite ({GetPlanetName(des[4])})";
                return info;
            }

            // Check for packed permanent
            if (len == 5)
            {
                if (des[0] == '~')
                {
                    info.Format = Format.Packed;
                    info.Type = DesignationType.Permanent;
                    info.Subtype = "permanent numbered (tilde/base-62, >= 620000)";
                    return info;
                }

                if (des.All(char.IsDigit))
                {
                    info.Format = Format.Packed;
                    info.Type = DesignationType.Permanent;
                    info.Subtype = "permanent numbered (5-digit, < 100000)";
                    return info;
                }

                if (char.IsLetter(des[0]) && des.Skip(1).All(char.IsDigit))
                {
                    info.Format = Format.Packed;
                    info.Type = DesignationType.Permanent;
                    info.Subtype = char.IsUpper(des[0])
                        ? "permanent numbered (letter-prefix, 100000-359999)"
                        : "permanent numbered (letter-prefix, 360000-619999)";
                    return info;
                }

                // Check for packed numbered comet
                if (des.Take(4).All(char.IsDigit) && (des[4] == 'P' || des[4] == 'D'))
                {
                    info.Format = Format.Packed;
                    info.Type = DesignationType.CometNumbered;
                    info.Subtype = $"comet numbered {GetCometTypeName(des[4])}";
                    return info;
                }
            }

            // Check for packed provisional (7 chars)
            if (len == 7)
            {
                if (des[0] == '_')
                {
                    info.Format = Format.Packed;
                    info.Type = DesignationType.ProvisionalExtended;
                    info.Subtype = "provisional (extended format, cycle >=620)";
                    return info;
                }

                if (des.StartsWith("PLS"))
                {
                    info.Format = Format.Packed;
                    info.Type = DesignationType.Survey;
                    info.Subtype = "survey (Palomar-Leiden)";
                    return info;
                }

                if (des[0] == 'T' && des[2] == 'S' && des[1] >= '1' && des[1] <= '3')
                {
                    info.Format = Format.Packed;
                    info.Type = DesignationType.Survey;
                    info.Subtype = $"survey (Trojan T-{des[1]})";
                    return info;
                }

                if (des[0] >= 'A' && des[0] <= 'L' && char.IsDigit(des[1]) && char.IsDigit(des[2]) &&
                    char.IsUpper(des[3]) && char.IsUpper(des[6]))
                {
                    info.Format = Format.Packed;
                    info.Type = DesignationType.Provisional;
                    info.Subtype = "provisional";
                    return info;
                }

                if (des[0] >= 'I' && des[0] <= 'L' && char.IsDigit(des[1]) && char.IsDigit(des[2]) &&
                    char.IsUpper(des[3]) && (char.IsLower(des[6]) || des[6] == '0'))
                {
                    info.Format = Format.Packed;
                    info.Type = DesignationType.CometProvisional;
                    info.Subtype = "comet provisional";
                    return info;
                }
            }

            // --- UNPACKED FORMATS ---

            // Check for unpacked satellite
            var satMatch = Regex.Match(des, @"^S/\d{4} ([JSUN]) \d+$");
            if (satMatch.Success)
            {
                info.Format = Format.Unpacked;
                info.Type = DesignationType.Satellite;
                info.Subtype = $"natural satellite ({GetPlanetName(satMatch.Groups[1].Value[0])})";
                return info;
            }

            // Check for unpacked permanent (all digits)
            if (des.Length > 0 && des.All(char.IsDigit))
            {
                info.Format = Format.Unpacked;
                info.Type = DesignationType.Permanent;
                info.Subtype = "permanent numbered";
                return info;
            }

            // Check for unpacked survey
            var surveyMatch = Regex.Match(des, @"^\d+ (P-L|T-[123])$");
            if (surveyMatch.Success)
            {
                string survey = surveyMatch.Groups[1].Value;
                info.Format = Format.Unpacked;
                info.Type = DesignationType.Survey;
                info.Subtype = survey == "P-L" ? "survey (Palomar-Leiden)" : $"survey (Trojan {survey})";
                return info;
            }

            // Check for old-style asteroid
            if (Regex.IsMatch(des, @"^[AB]\d{3} [A-Z][A-Z]$"))
            {
                info.Format = Format.Unpacked;
                info.Type = DesignationType.Provisional;
                info.Subtype = "provisional (old-style pre-1925)";
                return info;
            }

            // Check for unpacked provisional asteroid
            if (Regex.IsMatch(des, @"^\d{4} [A-Z][A-Z]\d*$"))
            {
                info.Format = Format.Unpacked;
                info.Type = DesignationType.Provisional;
                info.Subtype = "provisional";
                return info;
            }

            // Check for unpacked comet with type prefix
            var cometFullMatch = Regex.Match(des, @"^(\d*)([PCDXAI])/(-?\d+) ([A-Z][A-Z0-9]+)(?:-([A-Z]{1,2}))?$");
            if (cometFullMatch.Success)
            {
                string numStr = cometFullMatch.Groups[1].Value;
                char ctype = cometFullMatch.Groups[2].Value[0];
                int year = int.Parse(cometFullMatch.Groups[3].Value);

                string yearDesc = "";
                if (year < 0) yearDesc = "BCE";
                else if (year < 1000) yearDesc = "ancient";

                string typeDesc = GetCometTypeName(ctype);

                info.Format = Format.Unpacked;
                info.Type = DesignationType.CometFull;
                if (!string.IsNullOrEmpty(numStr))
                {
                    info.Subtype = string.IsNullOrEmpty(yearDesc)
                        ? $"comet numbered with provisional ({typeDesc})"
                        : $"comet numbered with {yearDesc} provisional ({typeDesc})";
                }
                else
                {
                    info.Subtype = string.IsNullOrEmpty(yearDesc)
                        ? $"comet provisional ({typeDesc})"
                        : $"comet {yearDesc} provisional ({typeDesc})";
                }
                return info;
            }

            // Check for unpacked numbered comet
            var numberedMatch = Regex.Match(des, @"^(\d+)([PD])(?:/[A-Za-z].*)?$");
            if (numberedMatch.Success)
            {
                char cometType = numberedMatch.Groups[2].Value[0];
                info.Format = Format.Unpacked;
                info.Type = DesignationType.CometNumbered;
                info.Subtype = $"comet numbered {GetCometTypeName(cometType)}";
                return info;
            }

            throw new MPCDesignationException($"Unable to detect designation format: {designation}");
        }

        // =========================================================================
        // Main conversion functions
        // =========================================================================

        public static ConversionResult Convert(string designation)
        {
            var info = DetectFormat(designation);
            string output;

            if (info.Format == Format.Packed)
            {
                output = info.Type switch
                {
                    DesignationType.Permanent => UnpackPermanent(designation).ToString(),
                    DesignationType.Provisional or DesignationType.Survey or DesignationType.ProvisionalExtended
                        => UnpackProvisional(designation),
                    DesignationType.CometNumbered => UnpackCometNumbered(designation),
                    DesignationType.CometProvisional => UnpackCometProvisional(designation),
                    DesignationType.CometFull => UnpackCometFull(designation),
                    DesignationType.CometAncient or DesignationType.CometBCE => UnpackAncientComet(designation),
                    DesignationType.Satellite => UnpackSatellite(designation),
                    _ => throw new MPCDesignationException("Unknown type")
                };
            }
            else
            {
                string trimmed = Trim(designation);
                output = info.Type switch
                {
                    DesignationType.Permanent => PackPermanentFromString(trimmed),
                    DesignationType.Provisional or DesignationType.Survey => PackProvisional(designation),
                    DesignationType.CometNumbered => PackCometNumbered(designation),
                    DesignationType.CometFull => PackCometFull(designation),
                    DesignationType.Satellite => PackSatellite(designation),
                    _ => throw new MPCDesignationException("Unknown type")
                };
            }

            return new ConversionResult { Input = designation, Output = output, Info = info };
        }

        private static string PackPermanentFromString(string s)
        {
            if (!long.TryParse(s, out long num) || num < 1 || num > MAX_ASTEROID_NUMBER)
            {
                throw new MPCDesignationException($"Invalid asteroid number: {s}");
            }
            return PackPermanent(num);
        }

        public static string ConvertSimple(string designation)
        {
            return Convert(designation).Output;
        }

        public static string Pack(string designation)
        {
            var info = DetectFormat(designation);
            if (info.Format == Format.Packed)
            {
                return Trim(designation);
            }
            return Convert(designation).Output;
        }

        public static string Unpack(string designation)
        {
            var info = DetectFormat(designation);
            if (info.Format == Format.Unpacked)
            {
                return Trim(designation);
            }
            return Convert(designation).Output;
        }

        public static bool IsValid(string designation)
        {
            if (string.IsNullOrEmpty(designation)) return false;
            try
            {
                DetectFormat(designation);
                return true;
            }
            catch
            {
                return false;
            }
        }
    }
}
