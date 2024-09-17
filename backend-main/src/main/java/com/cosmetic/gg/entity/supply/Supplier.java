package com.cosmetic.gg.entity.supply;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Table;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

import com.cosmetic.gg.common.enums.EStatus;
import com.cosmetic.gg.entity.EntityBase;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@AllArgsConstructor
@NoArgsConstructor
@Table(name = "supplier")
@Entity
@Getter
@Setter
public class Supplier extends EntityBase{
	
	@NotNull(message = "Code can not null")
	@Column(name = "code", unique = true)
	private String code;
	
	@NotNull(message = "Name can not null")
	@Column(name = "name")
	private String name;
	
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
	
	@Column(name = "country")
	private String country;
	
	@Column(name = "status")
	@Enumerated(EnumType.STRING)
	private EStatus status;
	
	@Column(name = "description")
	private String description;
}
