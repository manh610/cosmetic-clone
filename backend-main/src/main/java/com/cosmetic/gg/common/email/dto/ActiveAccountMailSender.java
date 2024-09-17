package com.cosmetic.gg.common.email.dto;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter @Setter
@AllArgsConstructor
@NoArgsConstructor
public class ActiveAccountMailSender {
	private String id;
	private String email;
	private String username;
}
