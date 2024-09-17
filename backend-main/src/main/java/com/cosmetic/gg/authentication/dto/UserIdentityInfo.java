package com.cosmetic.gg.authentication.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class UserIdentityInfo {

	private String id;
	
	private String username;
	
	private String password;
}
