package com.cosmetic.gg.common.utils;

import com.cosmetic.gg.authentication.dto.UserIdentityInfo;

public class SecurityUserUtils {

private static ThreadLocal<UserIdentityInfo> currentUser = new InheritableThreadLocal<>();
	
	public static UserIdentityInfo getCurrentUser() {
	    return currentUser.get();
	}
	
	public static void setCurrentUser(UserIdentityInfo user) {
	    clearCurrentUser();
	    currentUser.set(user);
	}
	
	public static void clearCurrentUser() {
	    currentUser.remove();
	}
}
