package com.cosmetic.gg.model.product;

import java.time.LocalDateTime;

import javax.persistence.Lob;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

import com.cosmetic.gg.common.enums.EStatus;
import com.cosmetic.gg.model.BaseModel;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter @Setter
@AllArgsConstructor
@NoArgsConstructor
public class BrandModel extends BaseModel{

	@NotNull(message = "Code can not null")
	@NotEmpty(message = "Code can not empty")
	@Size(max = 50, message = "Max length is 50 characters")
	@Pattern(regexp = "^[a-zA-Z0-9_.-]*$", message = "Code only contains character, number, special character _-.")
	private String code;
	
	@NotEmpty(message = "Name can be not empty")
	@NotNull(message = "Name can not null")
	@Size(max = 700, message = "Max length is 700 characters")
	private String name;
	
	@NotNull(message = "Country can not null")
	private String country;
	
	@Lob
	private byte[] logo;
	
	private String slogan;
	
	private boolean isMall = false;
	
	private EStatus status;
	
	private String description;
	
	private LocalDateTime createdAt;
	
	private String createdBy;
	
	private LocalDateTime updatedAt;
	
	private String updatedBy;
}
