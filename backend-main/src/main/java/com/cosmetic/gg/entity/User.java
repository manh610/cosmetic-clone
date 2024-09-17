package com.cosmetic.gg.entity;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.*;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

import com.cosmetic.gg.common.converter.StringListConverter;
import com.cosmetic.gg.common.enums.EGender;
import com.cosmetic.gg.common.enums.EUserRank;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@AllArgsConstructor
@NoArgsConstructor
@Table(name = "user", indexes = {
		@Index(name = "idx_user_username", columnList = "username"),
		@Index(name = "idx_user_given_name", columnList = "given_name"),
		@Index(name = "idx_user_family_name", columnList = "family_name"),
		@Index(name = "idx_user_phone", columnList = "phone"),
		@Index(name = "idx_user_email", columnList = "email"),
		@Index(name = "idx_user_citizen_number", columnList = "citizen_number")
})
@Entity
@Getter
@Setter
public class User extends EntityCommon{
	
	@Column(name = "username", length = 50, unique = true)
	@Size(max = 50, message = "Max length is 50 characters")
	@NotNull(message = "Given name can not be null")
	@NotEmpty(message = "Username can not be empty")
	@Pattern(
			regexp = "(^[a-zA-Z0-9_.-]*$)",
			message = "Username must contain character, number, special character _-. and max length 50 characters")
	private String username;
	
	@Column(name = "password")
	@Size(min = 6, message = "Min length is 6 characters")
	private String password;
	
	@Column(name = "email", length = 50, unique = true)
	@Size(max = 50, message = "Max length is 50 characters")
	@NotNull(message = "Username can not be null")
	@NotEmpty(message = "Email address can not be empty")
	@Pattern(
			regexp = "(^([a-zA-Z0-9_.+\\-])+@(([a-zA-Z0-9\\-])+\\.)+([a-zA-Z0-9]{2,4})+$)",
			message = "Email must contain character, number, special character _-. and max length 50 characters")
	private String email;
	
	@Column(name = "phone", length = 15, unique = true)
	@Size(max = 15, message = "Max length is 15 characters")
	@NotNull(message = "Phone number can not be null")
	@NotEmpty(message = "Phone number can not be empty")
	@Pattern(
			regexp = "(^\\d{10,15}$)",
			message = "Phone number must have min length 10 numbers and max length 15 numbers")
	private String phone;
	
	@Column(name = "citizen_number", length = 12, unique = true)
	private String citizenNumber;
	
	@Column(name = "given_name", length = 15)
	@Size(max = 15, message = "Max length is 15 characters")
	@NotNull(message = "Given name can not be null")
	@NotEmpty(message = "Given name can not be empty")
	private String givenName;
	
	@Column(name = "family_name", length = 50)
	@Size(max = 50, message = "Max length is 50 characters")
	@NotNull(message = "Family name can not be null")
	@NotEmpty(message = "Family name can not be empty")
	private String familyName;
	
	@Column(name = "gender")
	@Enumerated(EnumType.STRING)
	private EGender gender;
	
	@Column(name = "dob")
	private LocalDateTime dob;
	
	@Column(name = "country")
	private String country;
	
	@Column(name = "user_rank")
	@NotNull(message = "Rank of user can not be null")
	@Enumerated(EnumType.STRING)
	private EUserRank userRank;
	
	@Column(name = "avatar")
	@Lob
    private byte[] avatar;
	
	@Column(name = "role_id")
	@NotNull(message = "Role of user can not be null")
	private String roleId;
	
	@Column(name = "delivery_unit_id")
	private String deliveryUnitId;
	
	@Column(name = "fmc_tokens", columnDefinition = "text")
	@Convert(converter = StringListConverter.class)
	private List<String> fcmTokens = new ArrayList<>();
}
