package com.cosmetic.gg.model;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.Convert;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Lob;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;

import com.cosmetic.gg.common.converter.StringListConverter;
import com.cosmetic.gg.common.enums.EGender;
import com.cosmetic.gg.common.enums.EStatus;
import com.cosmetic.gg.common.enums.EUserRank;
import com.fasterxml.jackson.annotation.JsonFormat;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter @Setter
@AllArgsConstructor
@NoArgsConstructor
public class UserModel extends BaseModel{

	@NotEmpty(message = "Username can be not empty")
	@Pattern(
			regexp = "(^[a-zA-Z0-9_.-]*$)",
			message = "Username must contain character, number, special character _-. and max length 50 characters")
	private String username;
	
	@Pattern(
			regexp = "(^(?=.*[a-z])(?=.*[A-Z])(?=.*\\d)(?=.*[#$@!%&*?])[A-Za-z\\d#$@!%&*?]{8,30}$)",
			message = "Password must contain character, number, special character _-. and max length 50 characters")
	private String password = null;
	
	@NotEmpty(message = "Email address can not be empty")
	@Pattern(
			regexp = "(^([a-zA-Z0-9_.+\\-])+@(([a-zA-Z0-9\\-])+\\.)+([a-zA-Z0-9]{2,4})+$)",
			message = "Email must contain character, number, special character _-. and max length 50 characters")
	private String email;
	
	@NotEmpty(message = "Phone number can not be empty")
	@Pattern(
			regexp = "(^\\d{10,15}$)",
			message = "Phone number must have min length 10 numbers and max length 15 numbers")
	private String phone;
	
	private String citizenNumber;

	@NotEmpty(message = "Given name can not be empty")
	private String givenName;
	
	@NotEmpty(message = "Family name can not be empty")
	private String familyName;
	
	@Enumerated(EnumType.STRING)
	private EGender gender;
	
//	@JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss.SSSSSS")
	private LocalDateTime dob;

	private String country;
	
	@NotNull(message = "Rank of user can not be null")
	@Enumerated(EnumType.STRING)
	private EUserRank userRank;
	
	@Lob
    private byte[] avatar;

	private String roleId;
	
	private String roleName;
	
	private String deliveryUnitId;
	
	private String deliveryUnitName;
	
	private EStatus status;
	
	private String description;
	
	@Convert(converter = StringListConverter.class)
	private List<String> fcmTokens = new ArrayList<>();
}
