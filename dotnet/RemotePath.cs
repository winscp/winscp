using System;
using System.Globalization;
using System.Reflection;
using System.Runtime.InteropServices;

namespace WinSCP
{
    [Guid("70253534-C5DC-4EF3-9C98-65C57D79C324")]
    [ClassInterface(Constants.ClassInterface)]
    [ComVisible(true)]
    public sealed class RemotePath : IReflect
    {
        public static string EscapeFileMask(string fileMask)
        {
            if (fileMask == null)
            {
                throw new ArgumentNullException(nameof(fileMask));
            }
            int lastSlash = fileMask.LastIndexOf('/');
            string path = lastSlash > 0 ? fileMask.Substring(0, lastSlash + 1) : string.Empty;
            string mask = lastSlash > 0 ? fileMask.Substring(lastSlash + 1) : fileMask;
            // Keep in sync with EscapeFileMask in GenerateUrl.cpp
            mask = mask.Replace("[", "[[]").Replace("*", "[*]").Replace("?", "[?]").Replace("<", "<<").Replace(">", ">>");
            return path + mask;
        }

        public static string EscapeOperationMask(string operationMask)
        {
            if (operationMask == null)
            {
                throw new ArgumentNullException(nameof(operationMask));
            }
            int lastSlash = operationMask.LastIndexOf('/');
            string path = lastSlash > 0 ? operationMask.Substring(0, lastSlash + 1) : string.Empty;
            string mask = lastSlash > 0 ? operationMask.Substring(lastSlash + 1) : operationMask;
            mask = mask.Replace("\\", "\\\\").Replace("*", "\\*").Replace("?", "\\?");
            return path + mask;
        }

        [Obsolete("Use RemotePath.Combine method")]
        public static string CombinePaths(string path1, string path2)
        {
            return Combine(path1, path2);
        }

        public static string Combine(string path1, string path2)
        {
            if (path1 == null)
            {
                throw new ArgumentNullException(nameof(path1));
            }

            if (path2 == null)
            {
                throw new ArgumentNullException(nameof(path2));
            }

            string result;

            if (path2.StartsWith("/", StringComparison.Ordinal))
            {
                result = path2;
            }
            else
            {
                result =
                    path1 +
                    ((path1.Length == 0) || (path2.Length == 0) || path1.EndsWith("/", StringComparison.Ordinal) ? string.Empty : "/") +
                    path2;
            }
            return result;
        }

        public static string TranslateRemotePathToLocal(string remotePath, string remoteRoot, string localRoot)
        {
            if (remotePath == null)
            {
                throw new ArgumentNullException(nameof(remotePath));
            }

            if (remoteRoot == null)
            {
                throw new ArgumentNullException(nameof(remoteRoot));
            }

            if (localRoot == null)
            {
                throw new ArgumentNullException(nameof(localRoot));
            }

            localRoot = AddSeparator(localRoot, @"\");
            remoteRoot = AddSeparator(remoteRoot, "/");

            string localPath;
            // special case
            if (AddSeparator(remotePath, "/") == remoteRoot)
            {
                localPath = localRoot;
            }
            else
            {
                if (!remotePath.StartsWith(remoteRoot, StringComparison.Ordinal))
                {
                    throw new InvalidOperationException(string.Format(CultureInfo.CurrentCulture, "{0} does not start with {1}", remotePath, remoteRoot));
                }

                string subPath = remotePath.Substring(remoteRoot.Length);
                // can happen only when remoteRoot is empty
                if (subPath.StartsWith("/", StringComparison.Ordinal))
                {
                    subPath = subPath.Substring(1);
                }
                subPath = subPath.Replace('/', '\\');
                localPath = localRoot + subPath;
            }
            return localPath;
        }

        private static string AddSeparator(string path, string separator)
        {
            // not adding to empty root paths, because the path may not even start with slash
            if ((path.Length > 0) && !path.EndsWith(separator, StringComparison.Ordinal))
            {
                path += separator;
            }
            return path;
        }

        public static string TranslateLocalPathToRemote(string localPath, string localRoot, string remoteRoot)
        {
            if (localPath == null)
            {
                throw new ArgumentNullException(nameof(localPath));
            }

            if (localRoot == null)
            {
                throw new ArgumentNullException(nameof(localRoot));
            }

            if (remoteRoot == null)
            {
                throw new ArgumentNullException(nameof(remoteRoot));
            }

            localRoot = AddSeparator(localRoot, @"\");
            remoteRoot = AddSeparator(remoteRoot, "/");

            string remotePath;
            // special case
            if (AddSeparator(localPath, @"\") == localRoot)
            {
                remotePath = remoteRoot;
            }
            else
            {
                if (!localPath.StartsWith(localRoot, StringComparison.Ordinal))
                {
                    throw new InvalidOperationException(string.Format(CultureInfo.CurrentCulture, "{0} does not start with {1}", localPath, localRoot));
                }

                string subPath = localPath.Substring(localRoot.Length);
                // can happen only when localRoot is empty
                if (subPath.StartsWith("\\", StringComparison.Ordinal))
                {
                    subPath = subPath.Substring(1);
                }
                subPath = subPath.Replace('\\', '/');
                remotePath = remoteRoot + subPath;
            }
            return remotePath;
        }

        public static string GetDirectoryName(string path)
        {
            string result;
            if (path == null)
            {
                result = null;
            }
            else if (path.Length == 0)
            {
                throw new ArgumentException("Path cannot be empty", nameof(path));
            }
            else
            {
                int i = path.LastIndexOf('/');
                if (i < 0)
                {
                    result = string.Empty;
                }
                else if (i == 0)
                {
                    if (path.Length == 1)
                    {
                        result = null;
                    }
                    else
                    {
                        result = "/";
                    }
                }
                else
                {
                    result = path.Substring(0, i);
                }
            }
            return result;
        }

        public static string AddDirectorySeparator(string path)
        {
            if (string.IsNullOrEmpty(path))
            {
                throw new ArgumentException("Path cannot be empty", nameof(path));
            }

            if (!path.EndsWith("/", StringComparison.Ordinal))
            {
                path += "/";
            }

            return path;
        }

        public static string GetFileName(string path)
        {
            string result;
            if (string.IsNullOrEmpty(path))
            {
                result = null;
            }
            else
            {
                int i = path.LastIndexOf('/');
                if (i >= 0)
                {
                    result = path.Substring(i + 1);
                }
                else
                {
                    result = path;
                }
            }
            return result;
        }

        FieldInfo IReflect.GetField(string name, BindingFlags bindingAttr)
        {
            return GetType().GetField(name, bindingAttr);
        }

        FieldInfo[] IReflect.GetFields(BindingFlags bindingAttr)
        {
            return GetType().GetFields(bindingAttr);
        }

        MemberInfo[] IReflect.GetMember(string name, BindingFlags bindingAttr)
        {
            return GetType().GetMember(name, bindingAttr);
        }

        MemberInfo[] IReflect.GetMembers(BindingFlags bindingAttr)
        {
            return GetType().GetMembers(bindingAttr);
        }

        MethodInfo IReflect.GetMethod(string name, BindingFlags bindingAttr)
        {
            return GetType().GetMethod(name, bindingAttr);
        }

        MethodInfo IReflect.GetMethod(string name, BindingFlags bindingAttr, Binder binder, Type[] types, ParameterModifier[] modifiers)
        {
            return GetType().GetMethod(name, bindingAttr, binder, types, modifiers);
        }

        MethodInfo[] IReflect.GetMethods(BindingFlags bindingAttr)
        {
            return GetType().GetMethods(bindingAttr);
        }

        PropertyInfo[] IReflect.GetProperties(BindingFlags bindingAttr)
        {
            return GetType().GetProperties(bindingAttr);
        }

        PropertyInfo IReflect.GetProperty(string name, BindingFlags bindingAttr, Binder binder, Type returnType, Type[] types, ParameterModifier[] modifiers)
        {
            return GetType().GetProperty(name, bindingAttr, binder, returnType, types, modifiers);
        }

        PropertyInfo IReflect.GetProperty(string name, BindingFlags bindingAttr)
        {
            return GetType().GetProperty(name, bindingAttr);
        }

        object IReflect.InvokeMember(string name, BindingFlags invokeAttr, Binder binder, object target, object[] args, ParameterModifier[] modifiers, CultureInfo culture, string[] namedParameters)
        {
            if (target == null)
            {
                throw new ArgumentNullException(nameof(target));
            }

            Type type = target.GetType();

            // This trivial implementation allows calling static methods over COM
            return type.InvokeMember(name, invokeAttr, binder, target, args, modifiers, culture, namedParameters);
        }

        Type IReflect.UnderlyingSystemType
        {
            get { return GetType(); }
        }
    }
}
